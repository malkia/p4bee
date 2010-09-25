(defpackage "P4BEE" (:use "CL"))
(in-package "P4BEE")

(defvar *default-environment-variables*
  '(("P4PORT" "public.perforce.com:1666")
    ("P4USER" "malkia")
    ("P4CLIENT" "malkia"))
  "Default builtin variables to ease the development. Normally we should store this in registry/configuration file and use them from there")

(defun set-environment-variables (&optional (environment-variables *default-environment-variables*))
  "Sets environment variables in the currently running process"
  (mapcar (lambda (item)
            (setf (lw:environment-variable (first item))
                  (second item)))
          environment-variables))

(set-environment-variables)

(defvar *product-registry-path* (sys:product-registry-path :p4bee))
(defvar *p4* #+mac "/opt/local/bin/p4" #-mac "p4")

(defmacro with-pipe ((stream &rest cmdline) &body body)
  "Opens a pipe, by executing the 'command'. Captures the output in 'stream'. Expands the 'body'. At the end closes the pipe."
  `(let ((,stream (sys:open-pipe (list ,@cmdline))))
     (prog1 ,@body
       (close ,stream))))

(defun slurp (stream)
  "Reads the stream line by line, and returns it as a list of lines"
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defun strcat (&rest strings)
  "Concatenates all of the strings into one, and returns it"
  (apply 'concatenate 'string strings))

(defun starts-with-p (text word)
  "Returns t if 'text' starts with 'word'. NIL otherwise"
  (string= word (subseq text 0 (min (length text) (length word)))))

(defun ends-with-p (text word)
  "Returns t if 'text' ends with 'word'. NIL otherwise"
  (string= word (subseq text (- (length text) (min (length text) (length word))))))

(defun split (string)
  "Splits a line delimited by space into list of tokens"
  (lw:split-sequence " " string :coalesce-separators t))

(defun split-lines (lines)
  "Splits lines, where each is delimited by space into list of list of tokens"
  (mapcar 'split lines))

(defun merge-lines (lines)
  (when lines
    (reduce (lambda (line1 line2)
              (concatenate 'string line1 (string #\newline) line2))
            lines)))

(defun format-time (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (format nil "~A ~D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D" (elt sys::*week-days* day) year month date hour minute second)))

(defun string-trim-nil (bag line)
  (when line
    (setf line (string-trim bag line))
    (when (> (length line) 0)
      line)))

;;; Perforce specific macros & functions

(defvar *p4-log* (make-string-output-stream))

(defmacro with-p4 ((stream &rest cmdline) &body body)
  `(let ((utime (get-universal-time))
         (rtime (get-internal-real-time)))
     (progn 
         (format *p4-log* "~A> ~A" (format-time utime) (list ,@cmdline))
         (finish-output *p4-log*)
         (prog1 (with-pipe (,stream *p4* ,@cmdline)
                  ,@body)
           (format *p4-log* "  [~,2Fs]~&"
                   (coerce (/ (- (get-internal-real-time) rtime)
                              internal-time-units-per-second)
                           'double-float))))))

(defmacro run-p4 (&rest cmdline)
  `(let ((utime (get-universal-time))
         (rtime (get-internal-real-time))
         (output))
     (format *p4-log* "~A> ~A" (format-time utime) ,@cmdline)
     (finish-output *p4-log*)
     (with-pipe (stream *p4* "-s" ,@cmdline)
       (setf output
             (loop for line = (read-line stream nil nil)
                   while line
                   do (format t ".")
                   do (finish-output *standard-output*)
                   collect (let* ((space (position #\Space line))
                                  (tag (subseq line 0 (1- space)))
                                  (rest (subseq line (1+ space))))
                             (list tag rest)))))
     (format *p4-log* " [~,2Fs]~&"
             (coerce (/ (- (get-internal-real-time) rtime)
                        internal-time-units-per-second)
                     'double-float))
     output))

         
(defun parse-info-line (line)
  "Parse info line produced by 'p4 info'"
  (let ((p (position #\: line)))
    (if (null p)
        (format t "line=~A" line)
      (list (intern (string-upcase (substitute #\- #\Space (subseq line 0 p))) "KEYWORD")
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

(defmacro p4 (&rest cmdline)
  (let ((s (gensym)))
    `(with-p4 (s ,@cmdline)
       (slurp s))))

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
          :title "Monitor"
          :columns (column-titles "ID" "A" "User" "Time" "Operation"))
   (refresh capi:push-button
            :callback-type :interface
            :callback 'refresh-monitor
            :text "Refresh"))
  (:default-initargs
   :visible-min-width 64
   :visible-min-height 64))

(defmethod refresh-monitor ((monitor-interface monitor-interface))
  (with-slots (panel) monitor-interface
    (setf (capi:collection-items panel)
          (split-lines (p4 "monitor" "show")))))  

(defmethod initialize-instance :after ((monitor-interface monitor-interface) &rest rest)
  (refresh-monitor monitor-interface)
  (with-slots (auto-refresh-timer) monitor-interface
    (mp:schedule-timer-relative
     (setf auto-refresh-timer
           (mp:make-timer 'capi:execute-with-interface-if-alive
                          monitor-interface 'refresh-monitor monitor-interface))
     1 5)))

(capi:define-interface clientspec-interface ()
  ()
  (:panes
   (panel capi:multi-column-list-panel
          :title "Clientspec"
          :columns (column-titles "Key" "Value")))
  (:default-initargs
   :visible-min-width 64
   :visible-min-height 64))

(defmethod initialize-instance :after ((clientspec-interface clientspec-interface) &rest rest)
  (with-slots (panel) clientspec-interface
    (setf (capi:collection-items panel)
          (parse-clientspec (p4 "client" "-o")))))

(capi:define-interface info-interface ()
  ()
  (:panes
   (panel capi:multi-column-list-panel
          :title "Info"
          :columns (column-titles "Key" "Value")))
  (:default-initargs
   :visible-min-width 64
   :visible-min-height 64))

(defmethod initialize-instance :after ((info-interface info-interface) &rest rest)
  (with-slots (panel) info-interface
    (setf (capi:collection-items panel)
          (p4-info))))

(capi:define-interface log-interface ()
  ()
  (:panes
   (log capi:collector-pane
        :enabled :read-only
        :title "log"))
  (:default-initargs
   :visible-min-height 64
   :visible-min-width 64))

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
          :title "Changelists"
          :columns (column-titles "Changelist" "Date" "User" "Type" "Description")
          :callback-type '(:data :interface)
          :selection-callback (lambda (data interface)
                                (setf (slot-value interface 'changelist)
                                      (first data))
                                (refresh-details interface)))
   (details capi:multi-line-text-input-pane
            ;;:title "Description"
            :text "blah")
   (refresh capi:push-button
            :callback-type :interface
            :callback 'refresh-submitted-changelists
            :text "Refresh"))
  (:layouts
   (layout1 capi:column-layout '(panel :divider details))
   (layout2 capi:column-layout '(layout1 refresh)))
  (:default-initargs
   :visible-min-width 640
   :visible-min-height 280))

(defmethod refresh-details ((submitted-changelists-interface submitted-changelists-interface))
  (with-slots (details changelist) submitted-changelists-interface
    (setf (capi:text-input-pane-text details)
          (merge-lines (p4 "describe" "-s" changelist)))))

(defmethod refresh-submitted-changelists ((submitted-changelists-interface submitted-changelists-interface))
  (with-slots (panel) submitted-changelists-interface
    (setf (capi:collection-items panel)
          (merge-changelist-lines-with-descriptions (p4 "changes" "-m 100" "-t" "-l")))))
                    
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
   (depot depot-interface)
   (clientspec clientspec-interface)
   (info info-interface)
   (changes submitted-changelists-interface)
   (log log-interface))
  (:layouts
   (main capi:column-layout
         '(lay3 :divider lay1))
   (lay3 capi:row-layout
         '(depot :divider changes))
   (lay2 capi:column-layout
         '(clientspec :divider info))
   (lay1 capi:row-layout
         '(log :divider monitor :divider lay2)))
  (:default-initargs
   :title "p4bee"
   :message-area t
   :auto-menus t
   :menu-bar-items (list (make-instance 'capi:menu :title "Menu" :items '(1 2 3)))
   :display-state :maximized
   :visible-min-width 1920
   :visible-min-height 1100))

(defun combined-test ()
  (capi:display (make-instance 'combined-test-interface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (combined-test))

(defvar *clientspec* (p4 "client" "-o"))

(defun parse-clientspec (&optional (lines *clientspec*))
  (loop with bag = nil
        with filter = '(#\Space #\Tab)
        with key = nil
        with values = nil
        for line1 in lines
        for comment = (position #\# line1)
        for line = (if comment (subseq line1 0 comment) line1)
        for colon = (position #\: line)
        for line-key = (string-trim-nil filter (when colon (subseq line 0 colon)))
        for line-val = (string-trim-nil filter (if colon (subseq line (1+ colon)) line))
        when line-key do (progn (when key
                                  (pushnew (cons key values) bag))
                           (setf key (intern (string-upcase line-key) "KEYWORD")
                                 values nil))
        do (when line-val
             (setf line-val (case key
                              ((:VIEW :OPTIONS) (split line-val))
                              (otherwise line-val)))
             (pushnew line-val values))
        finally (when key (pushnew (cons key values) bag))
        finally return bag))

(capi:define-interface depot-interface ()
  ()
  (:panes
   (panel capi:extended-selection-tree-view
          :title "Depot View"
          :children-function    'depot-children-function
          :leaf-node-p-function 'depot-leaf-node-p-function
          :expandp-function     'depot-expandp-function
          :print-function       'depot-print-function
          :selection-callback   'depot-action-callback
          :checkbox-status       nil)
   (preview capi:multi-line-text-input-pane
            :horizontal-scroll t
            :vertical-scroll t))
  (:layouts
   (main capi:row-layout '(panel :divider preview)))
  (:default-initargs
   :visible-min-width 128
   :visible-min-height 64))

;; I need to encode in the actual item the data, 
;; :roots would need to be initialized with it
;; and depot-print-function would strip these out and return normal printout
;;
;; (list depot-interface item status text)
;;
;; So when we do depot-children-function
;; It would automatically return (list depot-interface item :WORKING "Retrieveing: Blah Blah")
;; Once this operation is finished (it's done in parallel), it would update the item

(defun depot-action-callback (item interface)
  (let ((position (search " - " item)))
  (setf (capi:text-input-pane-text (slot-value interface 'preview))
        (merge-lines (p4 "print" "-q" (subseq item 0 position))))))

(defun depot-print-function (duh)
  (when (listp duh)
      (setf duh (first duh)))
  (let ((slash (position #\/ duh :from-end t))
        (sharp (position #\# duh :from-end t)))
    (subseq duh (if slash (1+ slash) 0) sharp)))

(defun depot-leaf-node-p-function (duh)
  (search " - " duh))

(defun remove-ellipsis (string)
  (when (ends-with-p string "...")
    (subseq string 0 (- (length string) 3))))

(defun remove-end (string what-end)
  (if (ends-with-p string what-end)
      (subseq string 0 (- (length string) (length what-end)))
    string))

(defun filter-no-such-files (list)
  (dolist (item list)
    (when (ends-with-p item " - no such file(s).")
      (return-from filter-no-such-files)))
  list)

(defun depot-children-function (duh)
  (format t "~A~&" duh)
  (prog1
    (list (format nil "~A" duh))
    (let ((dirs (filter-no-such-files (p4 "dirs" "-C" duh "/*"))))
      (when dirs
        (let ((files (filter-no-such-files (p4 "files" duh "/*"))))
          (append dirs files))))))
          
(defun depot-expandp-function (duh)
  nil)

(defmacro mp-run (&body body)
  `(mp:process-run-function
    "mp-run" nil
    (lambda ()
      ,@body)))

;; The async update pattern 
;; 
;; mp:run-process
;;  (lambda () (let (results (calculate-data-which-takes-time)))
;;    (when-results-are-collected - 
;;         execute-with-interface -> set the data)
;; But also we have to keep storing the depot/interface in there

(defmethod initialize-instance :after ((depot-interface depot-interface) &rest rest)
  (with-slots (panel) depot-interface
    (mp-run
      ;; The job needs to be done between mp-run AND capi:execute-with-interface
      (let ((results (mapcar (lambda (x) (list (remove-end x "/...") "Blah"))
                             (mapcar 'first (rest (assoc :VIEW (parse-clientspec (p4 "client" "-o"))))))))
        ;; The job is finished. Now we can fetch the results back to the pane.
        (capi:execute-with-interface
         depot-interface
         (lambda () (setf (capi:tree-view-roots panel) results)))))))
#|
                                               
                (lambda (x)
                  (capi:apply-in-pane-process

          (mapcar (lambda (x) (list (remove-end x "/...") "Blah"))
                  (mapcar 'first (rest (assoc :VIEW (parse-clientspec (p4 "client" "-o")))))))))
|#

(capi:define-interface configuration-interface ()
  ()
  (:panes
   (port capi:text-input-pane :title "Port")
   (host capi:text-input-pane :title "Host")
   (client capi:text-input-pane :title "Client")
   (okay capi:push-button :text "OK")
   (cancel capi:push-button :text "Cancel"))
  (:layouts
   (main capi:column-layout '(lay1 lay2))
   (lay1 capi:column-layout '(port host client))
   (lay2 capi:row-layout '(okay cancel)))
  (:default-initargs
   :title "Perforce Configuration"
   :visible-min-width 256
;;   :visible-min-height 128
))

#+nil
(defmethod initialize-instance :after ((configuration-interface configuration-interface))
  (mapcar (lambda (name)
            (setf (slot-value configuration-interface key)
                  (lw:user-preference "config" (format nil "~A" key) :product :p4bee)))
          '(port host client)))
          
(defun config ()
  (capi:contain 'configuration-interface))

;; (list "Working" (lambda () (worker)))
;;