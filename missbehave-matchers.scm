;; 
;; %%HEADER%%
;; 

(module missbehave-matchers
( expect-at-least-n-applications
  expect-at-most-n-applications
  expect-exactly-n-applications
  ignore-arguments 
  match-arguments
  make-call-matcher
  message-from-predicate-form
  be is close-to any-of none-of
  list-including
  have-type
  match-string
  matches-string
  call
  calls
  raise
  make-error-matcher
  have has have-matcher
  )

(import chicken scheme extras data-structures irregex ports)
(require-extension missbehave advice (only srfi-1 every) (only sequences size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedure-Expections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (format-times n)
  (if (= n 1) "once" (sprintf "~A times" n))) 

(define ((expect-at-least-n-applications proc n) applications)
  (values
   (>= applications n)
   (sprintf "Expected ~A to be called at least ~A, but was called ~A" proc (format-times n) (format-times applications))))

(define ((expect-at-most-n-applications proc n) applications)
  (values
   (<= applications n)
   (sprintf "Expected ~A to be called at most ~A, but was called ~A" proc (format-times n) (format-times applications))))

(define ((expect-exactly-n-applications proc n) applications)
  (values
   (= applications n)
   (sprintf "Expected ~A to be called ~A, but was called ~A" proc (format-times n) (format-times applications))))

(define ((ignore-arguments) . _)
  (values  #t ""))

(define ((match-arguments proc . args) arguments)
  (values
   (equal? arguments args)
   (sprintf "Expected ~A to be called with ~A, but was called with ~A" proc args arguments)))


(define-syntax calls
  (syntax-rules (once twice times time never with)
    ((_ argument +more-arguments ...)
     (call arguments +more-arguments ...))))

(define-syntax call
  (syntax-rules (once twice times time never with)
    ((_ proc (with arg arg+ ...))
     (make-call-matcher proc
                        (expect-at-least-n-applications (quote proc) 1)
                        (match-arguments (quote proc) arg arg+ ...)))

    ((_ proc (with arg arg+ ...) once)
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) 1)
                        (match-arguments (quote proc) arg arg+ ...)))
    ((_ proc (with arg arg+ ...) twice)
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) 2)
                        (match-arguments (quote proc) arg arg+ ...)))

    ((_ proc (with arg arg+ ...) never)
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) 0)
                        (match-arguments (quote proc) arg arg+ ...)))

    ((_ proc (with arg arg+ ...) (n time))
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) n)
                        (match-arguments (quote proc) arg arg+ ...)))

    ((_ proc (with arg arg+ ...) (n times))
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) n)
                        (match-arguments (quote proc) arg arg+ ...)))
    ((_ proc never)
     (call proc (0 times)))
    ((_ proc once)
     (call proc (1 time)))
    ((_ proc twice)
     (call proc (2 times)))
    ((_ proc (n times))
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) n)
                        (ignore-arguments)))
    ((_ proc (n time))
     (make-call-matcher proc
                        (expect-exactly-n-applications (quote proc) n)
                        (ignore-arguments)))))




(define (make-call-matcher  procedure application-count-matcher argument-matcher)
  (let* ((applications 0)
         (arguments (unspecified))
         (counting-advice (lambda (proc  args)
                            (set! applications (+ 1 applications))
                            (apply proc args)))
         (arguments-advice (lambda (proc  args)
                             (set! arguments args)
                             (apply proc args))))
    (make-matcher
	 (let ((counting-advice-id #f)
		   (arguments-advice-id #f))
	   (lambda (subject)
		 (dynamic-wind
           (lambda ()
             (advise 'around procedure counting-advice)
             (advise 'around procedure arguments-advice))
           (lambda ()
             (force subject)
             (and (application-count-matcher applications) (argument-matcher arguments)))
           (lambda ()
			 (and counting-advice-id (unadvise procedure counting-advice-id))
			 (and arguments-advice-id (unadvise procedure arguments-advice-id))))))
     (lambda (form subject negate)
       (receive (count-matched count-message) (application-count-matcher applications)
         (receive (arguments-matched argument-message) (argument-matcher arguments)
           (cond
            ((not count-matched)
             (if (not arguments-matched)
                 (sprintf "~A.  Additionally: ~A" count-message argument-message)
                 (sprintf "~A" count-message)))
            (else (sprintf "~A" argument-message)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (match-string what #!key (with-matches #f))
  (let ((mismatch #f))
    (matcher
     (check (subject)
       (if with-matches
           (let ((matches (irregex-match (force subject) what)))
             (call-with-current-continuation
              (lambda (return)
                (for-each
                 (lambda (submatch)
                   (unless matches
                     (return #f))
                   (unless (and  (irregex-match-valid-index? matches (car submatch))
                                 (equal? (irregex-match-substring matches (car submatch)) (cdr submatch)))
                     (set! mismatch submatch)
                     
                     (return #f)))
                 with-matches)
                #t)))
           (irregex-match (force subject) what)))
     (message (form subject negate)
       (if with-matches
           (if mismatch
               (if negate
                   (sprintf "Expected ~A not to include submatch ~A when matched against ~S" form mismatch what)
                   (sprintf "Expected ~A to include submatch ~A when matched against ~S" form mismatch what))
               (if negate
                   (sprintf "Exepcted ~A not to match ~S" form what)
                   (sprintf "Expected ~A to match ~S" form what)))
           (if negate
               (sprintf "Expected ~A not to match ~S" form what)
               (sprintf "Expected ~A to match ~S" form what)))))))

(define matches-string match-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Be
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message-from-predicate-form form)
  (if (list? form)
      (let ((name (symbol->string (car form))))
        (with-output-to-string
          (lambda ()
            (display (string-translate name "-" " "))
            (display " ")
            (for-each (cut printf "~A " <>) (cdr form)))))
      form))

(define-syntax is
  (syntax-rules (a an true false)
    ((_ arguments +more-arguments ...)
     (be arguments +more-arguments ...))))

(define-syntax be
  (syntax-rules (a an true false)
    ((_ true)
     (be #t))
    ((_ false)
     (be #f))
    ((_ a type)
     (have-type type))
    ((_ an type)
     (have-type type))
    ((_ pred-or-value)
     (matcher
      (check (subject)
        (if (procedure? pred-or-value)
            (pred-or-value (force subject))
            (equal? pred-or-value (force subject))))
      (message (form subject negate)
        (if negate
            (if (procedure? pred-or-value)
                (sprintf "Expected ~S not to be ~A" (force subject) (message-from-predicate-form (quote pred-or-value)))
                (sprintf "Expected ~S not to be ~S" (force subject) pred-or-value))
            (if (procedure? pred-or-value)
                (sprintf "Expected ~S to be ~A" (force subject) (message-from-predicate-form (quote pred-or-value)))
                (sprintf "Expected ~S to be ~S" (force subject) pred-or-value))))))
    ((_ pred value more-values ...)
     (matcher
      (check (subject)
         (apply pred (list (force subject) value more-values ...)))
      (message (form subject negate)
        (with-output-to-string
          (lambda ()
            (if negate
                (printf "Expected ~S not to be ~S" (force subject) (quote pred))
                (printf "Expected ~S to be ~S" (force subject) (quote pred)))
            (for-each
             (lambda (val)
               (printf " ~S" val))
             (list value more-values ...)))))))))


(define-syntax have-type
  (lambda (form rename env)
    (let* ((type (cadr form))
           (type-pred (string->symbol (conc (symbol->string type) "?")))
           (%make-matcher (rename 'make-matcher)))
      `(,%make-matcher
        (lambda (subject)
          (,type-pred (force subject)))
        (lambda (form subject negate)
          (if negate
              (sprintf "Expected ~S to not be a ~A" (force subject) (quote ,type))
              (sprintf "Expected ~S to be a ~A" (force subject) (quote ,type))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Be helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((close-to what #!key (delta 0.3)) actual)
  (<= (abs (- what actual)) delta))

(define ((any-of item . more-items) subject)
  (member subject (cons item more-items)))

(define ((none-of item . more-items) subject)
  (not (member subject (cons item more-items))))

(define ((list-including item . more-items) subject)
  (and (list? subject)
       (every (cut member <> subject) (cons item more-items))))

(define ((vector-including item . more-items) subject) #t)
(define ((hash-table-including item . more-items) subject) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Have/Has
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax has
  (syntax-rules ()
    ((_ argument +more-arguments ...)
     (have argument +more-arguments ...))))

(define-syntax have
  (syntax-rules ()
    ((_ amount procedure-or-sugar)
     (if (procedure? (quote procedure-or-sugar))
         (have-matcher amount procedure-or-sugar (quote procedure-or-sugar))
         (have-matcher amount (quote procedure-or-sugar) (quote procedure-or-sugar))))))

(define (have-matcher expected-amount procedure-or-sugar procedure-or-sugar-name #!key (compare =))
  (let ((actual-amount #f))
    (matcher
     (check (subject)
            (let* ((collection (if (procedure? procedure-or-sugar) (procedure-or-sugar (force subject)) (force subject)))
                   (item-amount (size collection)))
              (set! actual-amount item-amount)
              (compare item-amount expected-amount)))
   
     (message (form subject negate)
              (if negate
                  (sprintf "Didn't expect ~A ~A" expected-amount procedure-or-sugar-name)
                  (sprintf "Expected ~A ~A but found ~A"  expected-amount procedure-or-sugar-name actual-amount))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; raise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax raise
  (syntax-rules (error errors with)
    ((_ error)
     (make-error-matcher))
    ((_ errors)
     (make-error-matcher))
    ((_ (kind more-kinds ...))
     (make-error-matcher kinds: '(kind more-kinds ...)))))

(define (make-error-matcher #!key (kinds #f) (properties #f))
  (let ((message "")
        (negative-message ""))
    (make-matcher
     (lambda (code)
       (handle-exceptions exn
           (let* ((condition (condition->list exn))
                  (exn-kinds (map car condition)))
             (cond
              ((and kinds properties) #t)
              (kinds
               (if (every (cut member <> exn-kinds) kinds)
                   #t
                   (begin
                     (set! message (sprintf "Expected exn of kinds ~A but got ~A" kinds exn-kinds))
                                        ;FIXME find proper wording
                     (set! negative-message (sprintf "Expected exn not of kinds ~A but got ~A" kinds exn-kinds))
                     #f)))
              (properties #t)
              (else
               (set! message            (sprintf "Expecte errors but didn't get one"))
               (set! negative-message   (sprintf "Expected no errors but got one"))
               #t)))
         (force code)
         #f))
     (lambda (form subject negate)
       (if negate
           negative-message
           message)))))

)
