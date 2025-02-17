(in-package "CL-USER")
(load-all-patches)

#+cocoa
(when (save-argument-real-p)
  (compile-file (example-file "configuration/macos-application-bundle") :output-file :temp :load t))

(let* ((application "p4bee")
       (main "main"))
  (compile-file (current-pathname application) :output-file :temp :load t)
  (deliver (find-symbol (string-upcase main) (string-upcase application))
           #+win32 (current-pathname (merge-pathnames application ".exe"))
           #+cocoa (write-macos-application-bundle
                    (current-pathname (merge-pathnames application ".app")) :document-types nil)
           5
           :interface :capi
           :keep-conditions :none
           :delete-packages '("HQN-WEB")
 ;          :call-count t
;           :clos-info :classes-and-methods
;           :interrogate-symbols t
           :numeric nil
           :keep-trans-numbers t
           :smash-packages (list "HQN-WEB" "SQL" "SQL-COMMON")
;           :manifest-file :no-common-controls-6
           :diagnostics-file (current-pathname (merge-pathnames application ".deliv"))
))
