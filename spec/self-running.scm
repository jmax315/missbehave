(use missbehave)

(run-specification
  (call-with-specification
   (make-empty-specification)
   (lambda () 
     (include "test.scm"))))
