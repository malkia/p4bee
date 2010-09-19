(defpackage "P4BEE" (:use "CL"))
(in-package "P4BEE")

(setf (lw:environment-variable "P4PORT") #+nil "bundy:1666" #-nil "public.perforce.com:1666")

;;; sys:open-pipe is Lispworks specific

(defmacro with-pipe ((stream command &rest rest) &body body)
  "Opens a pipe, by executing the 'command'. Captures the output in 'stream'. Expands the 'body'. At the end closes the pipe."
  `(let ((,stream (sys:open-pipe ,command ,@rest)))
     (prog1 ,@body
       (close ,stream))))

;; Questionable macro
(defmacro for-each-line ((stream line) what &body body)
  "Wrapper around 'for'. Iterates through each line while there is line and does 'what' to the 'body'"
  `(loop for ,line = (read-line ,stream nil nil)
         while ,line
         ,what ,@body))

(defun slurp (stream)
  "Reads the whole stream, and returns a list of lines"
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defun strcat (&rest strings)
  (apply 'concatenate 'string strings))

(defun split (string)
  "Splits a line delimited by space into list of tokens"
  (lw:split-sequence " " string :coalesce-separators t))

(defun split-lines (lines)
  "Splits lines, where each is delimited by space into list of list of tokens"
  (mapcar 'split lines))

(defun starts-with-p (text word)
  "Returns t if 'text' starts with 'word'. NIL otherwise'"
  (string= word (subseq text 0 (min (length text) (length word)))))

(defun merge-lines (lines)
  (reduce (lambda (line1 line2)
            (concatenate 'string line1 (string #\newline) line2))
          lines))

(defun format-time (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (format nil "~A/~A/~A ~A:~A:~A" year month day hour minute second)))

;;; Perforce specific macros & functions

(defvar *p4-log* (make-string-output-stream))

(defmacro with-p4 ((stream command &optional (arguments "") &rest rest) &body body)
  `(with-pipe (,stream (format nil #+mac "/opt/local/bin/p4 ~A ~A" #-mac "p4 ~A ~A" ,command ,arguments) ,@rest)
     (progn
       (format *p4-log* "~A> ~A ~A~&" (format-time) ,command ,arguments)
       ,@body)))

(defun parse-info-line (line)
  "Parse info line produced by 'p4 info'"
  (let ((p (position #\: line)))
    (if (null p)
        (format t "line=~A" line)
      (cons (intern (string-upcase (substitute #\- #\Space (subseq line 0 p))) "KEYWORD")
            (string-trim " " (subseq line (1+ p)))))))

(defun parse-changelist-line (line)
  "Parse changelist line produced by 'p4 changes -t'"
  (let ((words (split line)))
    (when (and (string= (nth 0 words) "Change")
               (string= (nth 2 words) "on")
               (string= (nth 5 words) "by"))
      (list (nth 1 words)          ; changelist
            (strcat (nth 3 words)  ; date
                    "  "
                    (nth 4 words)) ; time
            (nth 6 words)          ; user@clientspec
            (nth 7 words)))))      ; *pending*

(defun merge-changelist-lines-with-descriptions (lines)
  "Combines lines produced by 'p4 changes -l'"
  (loop with change = nil
        with newline = (string #\newline)
        with charbag = '(#\Space #\newline #\tab)
        with description = ""
        for line in lines
        for fline = (string-trim charbag line)
        for new-change-p = (starts-with-p line "Change ")
        when (and new-change-p change) collect change
        when new-change-p do (setf change nil)
        do (if change
               (when (> (length fline) 0)
                 (setf (first description)
                       (concatenate 'string (first description) fline newline)))
             (setf description (list "")
                   change (append (parse-changelist-line line) description)))))

(defun parse-describe-line (line)
  "Parse describe line produced by 'p4 describe -s CL#"
  (let ((words (split line)))
    (when (and (string= (nth 0 words) "Change")
               (string= (nth 2 words) "by")
               (string= (nth 4 words) "on"))
      (list (nth 1 words)              ; changelist
            (nth 3 words)              ; user@clientspec
            (strcat (nth 5 words)      ; date
                    "  "
                    (nth 6 words)))))) ; time

(defun parse-client-line (line)
  "Parse client line produced by 'p4 clients'"
  (let* ((words1 (lw:split-sequence "'" line :coalesce-separators t))
         (words  (split (first words1))))
    (when (string= (nth 0 words) "Client")
      (list (nth 1 words)                  ; clientspec
            (nth 2 words)                  ; date
            (nth 3 words)                  ; root
            (apply 'strcat (cddddr words)) ; dir (Does not work for C:\Documents and Settings)
            (second words1)))))            ; 'description'

(defun p4-info ()
  (with-p4 (s "info")
    (mapcar 'parse-info-line (slurp s))))

(defun p4 (command &optional (arguments ""))
  (with-p4 (s command arguments)
    (for-each-line (s line)
        collect line)))

;;; Some default information

(defvar *info* (p4-info))
(defvar *user* (cdr (assoc :USER-NAME *info*)))
(defvar *client* (cdr (assoc :CLIENT-NAME *info*)))
(defvar *root* (cdr (assoc :CLIENT-ROOT *info*)))
(defvar *host* (cdr (assoc :CLIENT-HOST *info*)))
(defvar *version* (cdr (assoc :SERVER-VERSION *info*)))

;;; Some CAPI stuff

(defmacro column-titles (&rest titles)
  (let ((title (gensym)))
    `(loop for ,title in (list ,@titles)
           collect (list :title ,title))))

;;; Simple "p4 monitor show" tabular display

(capi:define-interface monitor-interface ()
  (auto-refresh-timer)
  (:panes
   (panel capi:multi-column-list-panel
          :columns (column-titles "ID" "A" "User" "Time" "Operation"))
   (refresh capi:push-button
            :callback-type :interface
            :callback 'refresh-monitor
            :text "Refresh"))
  (:default-initargs
   :visible-min-width 320
   :visible-min-height 100))

(defmethod refresh-monitor ((monitor-interface monitor-interface))
  (with-slots (panel) monitor-interface
    (setf (capi:collection-items panel)
          (split-lines (p4 "monitor show")))))  

(defmethod initialize-instance :after ((monitor-interface monitor-interface) &rest rest)
  (refresh-monitor monitor-interface)
  (with-slots (auto-refresh-timer) monitor-interface
    (mp:schedule-timer-relative
     (setf auto-refresh-timer
           (mp:make-timer 'capi:execute-with-interface-if-alive
                          monitor-interface 'refresh-monitor monitor-interface))
     1 5)))

(capi:define-interface log-interface ()
  ()
  (:panes
   (log capi:collector-pane))
  (:default-initargs
   :visible-min-height 50
   :visible-min-width 300))

(defmethod initialize-instance :after ((log-interface log-interface) &rest rest)
  (with-slots (log) log-interface
    (setf *p4-log*
          (capi:collector-pane-stream log))))

(defun show-monitor ()
  (capi:display (make-instance 'monitor-interface)))

;;; 

(capi:define-interface submitted-changelists-interface ()
  (changelist)
  (:panes
   (panel capi:multi-column-list-panel
          :columns (column-titles "Changelist" "Date" "User" "Type" "Description")
          :callback-type '(:data :interface)
          :selection-callback (lambda (data interface)
                                (setf (slot-value interface 'changelist)
                                      (first data))
                                (refresh-details interface)))
   (details capi:multi-line-text-input-pane
            :text "blah")
   (refresh capi:push-button
            :callback-type :interface
            :callback 'refresh-submitted-changelists
            :text "Refresh"))
  (:layouts
   (layout1 capi:row-layout '(panel :divider details))
   (layout2 capi:column-layout '(layout1 refresh)))
  (:default-initargs
   :visible-min-width 640
   :visible-min-height 280))

(defmethod refresh-details ((submitted-changelists-interface submitted-changelists-interface))
  (with-slots (details changelist) submitted-changelists-interface
    (setf (capi:text-input-pane-text details)
          (merge-lines (p4 "describe -s" changelist)))))

(defmethod refresh-submitted-changelists ((submitted-changelists-interface submitted-changelists-interface))
  (with-slots (panel) submitted-changelists-interface
    (setf (capi:collection-items panel)
          (merge-changelist-lines-with-descriptions (p4 "changes -m 100 -t -l")))))
                    
(defmethod initialize-instance :after ((submitted-changelists-interface submitted-changelists-interface) &rest rest)
  (refresh-submitted-changelists submitted-changelists-interface))

(defun show-submitted-changelists ()
  (capi:display (make-instance 'submitted-changelists-interface)))

;;;

(capi:define-interface changelist-describe-interface ()
  ()
  (:panes
   (panel capi:multi-column-list-panel
          :columns (column-titles "Changelist" "ClientSpec" "Date"))))
   



(capi:define-interface combined-test-interface ()
  ()
  (:panes
   (monitor monitor-interface)
   (changes submitted-changelists-interface)
   (log log-interface))
  (:layouts
   (main capi:column-layout
         '(monitor changes :divider log)))
  (:default-initargs
   :visible-min-width 1024
   :visible-min-height 768))

(defun combined-test ()
  (capi:display (make-instance 'combined-test-interface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (combined-test))