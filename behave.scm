;; 
;; %%HEADER%%
;; 

(use missbehave fmt fmt-color data-structures chicken-syntax args posix files regex)
  
(define use-colors #t)

(define command-line-options
  (list (args:make-option (h help) #:none "Display this help" (usage port: (current-output-port) status: 0))
        (args:make-option (n nocolor) #:none "Don't display colors" (set! use-colors #f))
        (args:make-option (t tags) (required: "TAG") "Tags to filter. Can be used multiple times" )))

(define (usage #!key (port (current-error-port)) (status 1))
  (with-output-to-port port
    (lambda ()
      (print "Usage: " (car (argv)) " [options ...] file ...")
      (newline)
      (print (args:usage command-line-options))))
  (exit status))

(when (= 1 (length (argv)))
  (usage))
  

(define-record report-agenda failed successful pending failed-examples pending-examples indentation-factor)
(define-record failure result index)


(define (make-pretty-reporter)
  (let ((agenda (make-report-agenda  0  0  0 (list) #f  0)))
    (lambda (data #!key (mode 'adhoc))
      (case mode
        ((adhoc)   (report-adhoc! data agenda))
        ((summary) (report-summary! data agenda))))))

(define (count-parents context)
  (let loop ((parent (context-parent context)) (count 0))
    (cond
     (parent
      (loop (context-parent parent) (+ count 1)))
     (else count))))

(define (report-adhoc! data agenda)
  (cond
   ((context? data)
    (let* ((parents (count-parents data))
           (ifactor (if (or (not parents) (zero? parents)) 1 parents)))
      (report-agenda-indentation-factor-set! agenda parents)
      (if use-colors
          (indent (* 2 ifactor) (fmt #f (fmt-bold (context-description data))))
          (indent (* 2 ifactor) (fmt #f (context-description data))))
      (newline)))
   ((example? data) #t)
   ((example-result? data)
    (cond
     ((example-failed? data)
      (let ((failure-index (+ 1 (report-agenda-failed agenda))))
        (report-example 'failed data agenda failure-index)
        (report-agenda-failed-examples-set!
         agenda
         (cons (make-failure  data failure-index)
               (report-agenda-failed-examples agenda)))
        (agenda-increment-failed! agenda)))
     ((example-pending? data)
      (report-example 'pending data agenda)
      (agenda-increment-pending! agenda))
     (else
      (report-example 'success data agenda)
      (agenda-increment-successful! agenda))))))

(define (report-summary! data agenda)
  (if use-colors
      (report-summary-with-colors! data agenda)
      (report-summary-plain! data agenda)))

(define (report-summary-with-colors! data agenda)
  (let ((failure-count (report-agenda-failed agenda)))
    (unless (zero? failure-count)
      (newline)
      (newline)
      (if (= failure-count 1)
          (fmt #t (fmt-red (fmt-bold "There has been 1 failure ")))
          (fmt #t (fmt-red (cat (fmt-bold (cat "There have been " (report-agenda-failed agenda) " failures"))))))
      (newline)
      (newline)
      (report-failures (report-agenda-failed-examples agenda))))
  (newline)
  (newline)
  (fmt #t (cat
           (fmt-bold
            (cat
             "Total: "
             (+ (report-agenda-failed agenda)
                (report-agenda-pending agenda)
                (report-agenda-successful agenda))))
           " "
           (fmt-green (fmt-bold
                       (cat
                        "Successful: "
                        (report-agenda-successful agenda)
                        " ")))
           (fmt-yellow (fmt-bold
                        (cat
                         "Pending: "
                         (report-agenda-pending agenda)
                         " ")))
           (fmt-red (fmt-bold
                     (cat
                      "Failures: "
                      (report-agenda-failed agenda)
                      " ")))))
  (newline)
  (zero? (report-agenda-failed agenda)))

(define (report-summary-plain! data agenda)
  (let ((failure-count (report-agenda-failed agenda)))
    (unless (zero? failure-count)
      (newline)
      (newline)
      (if (= failure-count 1)
          (printf "There has been 1 failure~%")
          (printf "There have been ~A failures ~%" (report-agenda-failed agenda)))
      (newline))
    (report-failures (report-agenda-failed-examples agenda)))
  (newline)
  (newline)
  (printf "Total: ~A Successful: ~A Pending: ~A Failures: ~A"
    (+ (report-agenda-failed agenda)
       (report-agenda-pending agenda)
       (report-agenda-successful agenda))
    (report-agenda-successful agenda)
    (report-agenda-pending agenda)
    (report-agenda-failed agenda))
  (newline)
  (zero? (report-agenda-failed agenda)))

(define (report-example status result agenda #!optional (failure-index #f))
  (if use-colors
      (report-example-colors status result agenda failure-index)
      (report-example-plain status result agenda failure-index)))

(define (indent n str)
  (let ((indention (make-string n #\space)))
    (display indention)
    (display (irregex-replace/all "\n" str (string-append "\n" indention)))))

(define (report-example-colors status result agenda #!optional (failure-index #f))
  (let ((example (example-result-example result))
        (ifactor (report-agenda-indentation-factor agenda)))
    
    (if (or (not ifactor) (zero? ifactor))
        (set! ifactor 1))
    
    (case status
      ((success)
       (indent (* ifactor 3) (fmt #f (fmt-green (cat "It " (example-description example)))))
       (newline))
      ((pending)
       (indent (* ifactor 3) (fmt #f (fmt-yellow (cat "[P] It " (example-description example)))))
       (newline))
      (else
       (indent (* ifactor 3) (fmt #f (fmt-red (cat "[F][" (number->string failure-index) "] It " (example-description example) ))))
       (newline)
       (indent (* ifactor 4) (fmt #f (fmt-red (example-result-messages result))))
       (newline)))))

(define (report-example-plain status result agenda #!optional (failure-index #f))
  (let ((example (example-result-example result))
        (ifactor (report-agenda-indentation-factor agenda)))
    (case status
      ((success)
       (indent (* ifactor 3) (fmt #f (cat "It " (example-description example))))
       (newline))
      ((pending)
       (indent (* ifactor 3) (fmt #f (cat "[P] It " (example-description example))))
       (newline))
      (else
       (indent (* ifactor 3) (fmt #f (cat "[F][" (number->string failure-index) "] It " (example-description example))))
       (newline)
       (indent (* ifactor 4) (fmt #f (example-result-messages result)))
       (newline)))))


(define (report-failures failures)
  (for-each
   (if use-colors report-failure-colors report-failure-plain)
   (reverse failures)))

(define (report-failure-plain failure)
  (let* ((result (failure-result failure))
         (file (example-result-spec-file result))
         (example (example-result-example result)))

    (printf "~A) in ~A~%" (failure-index failure) (relativize-path file))
    (printf "[F][~A] It ~A~%" (failure-index failure) (example-description example))
    (printf "~A~%" (example-result-messages result))
    (newline)))

(define (report-failure-colors failure)
  (let* ((result (failure-result failure))
         (file (example-result-spec-file result))
         (example (example-result-example result)))

    (fmt #t (fmt-bold (fmt-red (cat (failure-index failure) ") in " (relativize-path file)))))
    (newline)
    (indent 2 (fmt #f (fmt-red (cat "[F][" (number->string (failure-index failure)) "] It " (example-description example)))))
    (newline)
    (indent 4 (fmt #f (fmt-red (example-result-messages result))))
    (newline)
    (newline)))

(define (agenda-increment-successful! agenda)
  (report-agenda-successful-set! agenda (+ 1 (report-agenda-successful agenda))))

(define (agenda-increment-failed! agenda)
  (report-agenda-failed-set! agenda (+ 1 (report-agenda-failed agenda))))

(define (agenda-increment-pending! agenda)
  (report-agenda-pending-set! agenda (+ 1 (report-agenda-pending agenda))))

(define (run-files files #!optional (include-filter #f) (exclude-filter #f))
  (run-specification
   (call-with-specification
    (make-empty-specification)
    (lambda () (for-each eval-spec-file files)))
   include: include-filter
   exclude: exclude-filter
   reporter: (make-pretty-reporter)))

(define (eval-spec-file file)
  (let ((content (read-file file)))
    (unless (null? content)
      (eval (decorate-content content file)))))

(define (decorate-content content file)
  `(begin
     (use missbehave missbehave-matchers)
     (parameterize ((current-spec-file ,file))
       ,@content)))

(define (absolutize-path path)
  (let ((cwd (current-directory)))
    (if (absolute-pathname? path)
        (normalize-pathname path)
        (normalize-pathname (conc cwd "/" path)))))

(define (relativize-path path)
  (pathname-strip-directory (string-substitute (regexp-escape (current-directory)) "" path)))

(define (extract-tags options)
  (fold (lambda (element tags)
          (if (eq? (car element) 't)
              (cons  (string-split (cdr element) ":") tags)
              tags))
        '()
        options))

(define (create-include-filter tags)
  (fold (lambda (tag filter)
          (let ((label (string-translate* (car tag) '(("@" . "")))))
            (if (not (equal? "~" (string-take label 1)))
                (if (= 1 (length tag))
                    (cons (list (string->symbol label)) filter)
                    (cons (list (string->symbol label) (string->symbol (cadr tag))) filter))
                filter)))
        '()
        tags))

(define (create-exclude-filter tags)
  (fold (lambda (tag filter)
          (let ((label (string-translate* (car tag) '(("@" . "")))))
            (if (equal? "~" (string-take label 1))
                (if (= 1 (length tag))
                    (cons (list (string->symbol (string-drop label 1))) filter)
                    (cons (list (string->symbol (string-drop label 1)) (string->symbol (cadr tag))) filter))
                filter)))
        '()
        tags))


(receive (options files) (args:parse (command-line-arguments) command-line-options)
  (let* ((tags (extract-tags options))
         (include-filter (create-include-filter tags))
         (exclude-filter (create-exclude-filter tags))
         (files-absolute (map absolutize-path files)))
    
    (for-each (lambda (file)
                (unless (file-exists? file)
                  (fprintf (current-error-port) "The spec-file ~A does not exist~%" file)
                  (exit 1)))
              files-absolute)
    
    (if (run-files
         files-absolute
         (if (null? include-filter) #f include-filter)
         (if (null? exclude-filter) #f exclude-filter))
        (exit 0)
        (exit 2))))












