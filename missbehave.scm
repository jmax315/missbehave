;; 
;; %%HEADER%%
;;
;;
;;
;;

(module missbehave
(
  run-specification behave call-with-specification make-empty-specification    
  describe context run-context call-with-context add-context-to-specification
  context-description create-context context? context-parent
  
  add-hook-to-context before after
  subject-set! subject 
 
  it create-example example-description run-example 
  add-example-to-context pending 

  example-result? example? example-result-example example-result-messages
  example-failed? example-pending? example-result-spec-file
  example-spec-file 
 
  expect run-expectation  to
  make-matcher
  matcher
 
  current-context context-subject-set!
  $ reset-state!
  current-spec-file
  negative-expectation
  do-not
  unspecified

  )

(import chicken scheme extras data-structures ports)
(require-extension
  srfi-1 regex
  (only srfi-69 hash-table-set! hash-table-ref/default hash-table-clear! make-hash-table))

(require-library matchable)
(import-for-syntax matchable)

(define-syntax returning
  (syntax-rules ()
    ((_ object code more-code ...)
     (let ((return-later object))
       code more-code ...
       return-later))))

(define (unspecified)
  (let ((intern 1))
    (set! intern 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specification
;;
;; A specification is the sum of all contexts/description with all
;; their examples. It's basically everything you have to say about the
;; behaviour of a particular object or system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-empty-specification) (list))

(define current-specification    (make-parameter (make-empty-specification)))

(define current-spec-file (make-parameter ""))

(define (call-with-specification specification thunk)
  (parameterize ((current-specification specification))
    (thunk)
    (current-specification)))

(define (run-specification specification #!key (include #f) (exclude #f) (reporter (make-standard-reporter)))
  (let ((contexts (filter-contexts specification include exclude)))
    (for-each (cut run-context <> reporter include exclude) contexts)
    (reporter #f mode: 'summary)))

(define (behave thunk)
  (if (run-specification
       (call-with-specification
        (make-empty-specification)
        thunk))
      0 1))

(define (filter-contexts contexts include exclude)
  (cond
   ((and (not include) (not exclude)) contexts)
   ((not exclude)
    (filter
     (lambda (ctx)
       (or
        (meta-matches? (context-meta ctx) include)
        (context-has-matching-examples? ctx include: include))) contexts))
   ((not include)
    (filter
     (lambda (ctx)
       (not (meta-matches? (context-meta ctx) exclude))) contexts))
   (else
    (filter
     (lambda (ctx)
       (let ((meta (context-meta ctx)))
         (and (meta-matches? meta include)
              (not (meta-matches? meta exclude)))))
     contexts))))

(define (context-has-matching-examples? context #!key (include #f) (exclude #f))
  (not (null? (filter-examples context include exclude))))

(define (add-context-to-specification context)
  (returning context
    (current-specification
      (cons context (current-specification)))))

(define (filter-examples context include exclude)  
  (let ((examples (context-examples context)))
    (cond
     ((and (not include) (not exclude)) examples)
     ((not exclude)
      (filter
       (lambda (example)
         (meta-matches? (example-meta example) include))
       examples))
     ((not include)
      (filter
       (lambda (example)
         (not (meta-matches? (example-meta example) exclude)))
       examples))
     (else
      (filter (lambda (example)
                (let ((meta (example-meta example)))
                  (and (meta-matches? meta include)
                       (not (meta-matches? meta exclude)))))
              examples)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts
;;
;; A context is means to scope your descriptions to a subsystem or a
;; specific aspect of the object you're describing.
;; Contexts can be nested. They can be decorated with meta-data which
;; can be used to filter them during execution. You can thus
;; selectively run specific contexts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record context description examples hooks meta subject parent)

(define current-context   (make-parameter #f))

(define (create-context description #!key (examples (list)) (meta (list)))
  (make-context description examples (list) meta #f (current-context)))

(define-record-printer (context ctx out)
  (fprintf
      out
      "#<context '~A' parent: ~A subject: ~A meta: ~A examples: ~A>"
    (context-description ctx)
    (context-parent ctx)
    (context-subject ctx)
    (context-meta ctx)
    (length (context-examples ctx))))

(define-syntax context
  (syntax-rules ()
    ((_ argument more-arguments ...)
     (describe argument more-arguments ...))))

(define-syntax describe
  (syntax-rules (meta)
    ((_ what)
     (add-context-to-specification
      (create-context what)))
    ((_ what (meta (tag value ...) ...))
     (add-context-to-specification
      (create-context what meta: '((tag value ...) ...))))
    ((_ what (meta (tag value ...) ...) example examples+ ...)
     (add-context-to-specification
      (call-with-context
       (create-context what meta: '((tag value ...) ...))
       (lambda () example examples+ ...))))
    ((_ what example examples+ ...)
     (add-context-to-specification
      (call-with-context
       (create-context what)
       (lambda ()  example examples+ ...))))))

(define (call-with-context context thunk)
  (parameterize ((current-context context))
    (returning (current-context)
       (thunk))))

(define (run-context context #!optional (reporter values) (include #f) (exclude #f))
  (reporter context mode: 'adhoc)
  (run-context-with-hooks context reporter include exclude))

(define (run-context-with-hooks context reporter include exclude)
  (let ((before-all  (find-context-hooks 'before 'all: context ))
        (after-all   (find-context-hooks 'after  'all: context ))
        (context-result #t))
    (parameterize (( current-context context))
      (call-with-hooks
       (lambda ()
         (for-each-example-in-context
          (lambda (example)
            (unless (run-example-with-hooks example context reporter)
              (set! context-result #f)))
          context
          include
          exclude))
       before-all
       after-all))
    context-result))

(define (for-each-example-in-context callback context include exclude)
  (for-each
   callback
   (reverse
    (cond
     ((and exclude include)
      (if (meta-matches? (context-meta context) include)
          (filter-examples context  #f exclude)
          (filter-examples context include exclude)))
     (include
      (if (meta-matches? (context-meta context) include)
          (filter-examples context #f exclude)
          (filter-examples context include #f)))
     (exclude     
      (filter-examples context #f exclude))
     (else  (context-examples context))))))

(define (run-example-with-hooks example context reporter)
  (reporter example mode: 'adhoc) 
  (let ((before-hooks-for-example (find-context-hooks-for-example 'before example context))
        (after-hooks-for-example  (find-context-hooks-for-example 'after  example context)))
    (call-with-hooks
     (lambda ()
       (let ((result (run-example example)))
         (reporter result mode: 'adhoc)
         (not (example-failed? result))))
     before-hooks-for-example
     after-hooks-for-example)))

(define (find-context-hooks type filter-exp context)
  (let ((hooks (all-hooks-with-parent-traversal context)))
    (map context-hook-hook
         (filter
          (lambda (hook)
            (and (eq? (context-hook-type hook) type )
                 (eq? (context-hook-filter hook) filter-exp)))
          hooks))))

(define (context-ancestors context)
  (let loop ((parent (context-parent context)) (ancestors '()))
    (cond
     ((not parent)  (append ancestors (list context)))
     (else (loop (context-parent parent) (cons parent ancestors))))))

(define (all-hooks-with-parent-traversal context)
  (append-map context-hooks (context-ancestors context)))

(define (subject-set! subject #!optional (context (current-context)))
  (when context
    (context-subject-set! context subject)))
                    
(define (subject)
  (when (current-context)
    (context-subject (current-context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-Hooks
;;
;; Hooks are procedures that are called at specific times
;; during the run of a context (before,after,...).
;; They typically hold setup and teardown code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record context-hook hook filter type)

(define-record-printer (context-hook hook out)
  (fprintf out
      "#<context-hook type(~A) filter(~A) hook(~A)>"
    (context-hook-type hook)
    (context-hook-filter hook)
    (context-hook-hook hook)))

(define-syntax before
  (syntax-rules (each: all:)
    ((_ each: code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'before filter: 'each:))
    ((_ all: code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'before filter: 'all:))
    ((_ filter code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'before filter: '(filter)))))

(define-syntax after
  (syntax-rules (each: all:)
    ((_ each: code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'after filter: 'each:))
    ((_ all: code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'after filter: 'all:))
    ((_ filter code more-code ...)
     (add-hook-to-context (lambda () code more-code ...) type: 'after filter: '(filter)))))

(define (add-hook-to-context hook  #!key (filter each:) (type 'before) (context (current-context)))
  (context-hooks-set! context
                      (cons
                       (make-context-hook hook filter type)
                       (context-hooks context))))

(define (call-with-hooks thunk before after)
  (dynamic-wind
      (lambda () (for-each (cut apply <> (list)) before))
      thunk
      (lambda () (for-each (cut apply <> (list)) after))))

(define (find-context-hooks-for-example type example context)
  (let ((hooks (all-hooks-with-parent-traversal context))
        (meta  (example-meta example)))
    (map context-hook-hook
         (filter
          (lambda (hook)
            (and (eq? (context-hook-type hook) type)
                 (meta-matches? meta (context-hook-filter hook))))
          hooks))))

(define (meta-matches? meta tags)
  (or (eq? tags each:)
      ((tag-expression->filter tags) meta)))

(define ((tag-expression->filter tags) meta)
  (every (lambda (tag) (member tag meta)) tags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;
;; Examples are pieces of behaviour you want to describe for a given
;; object or system. Use them to specify how your "subject" should
;; behave like. An example is usually set of expecations that are run
;; against the parts of your code you want to specify.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-exit-continuation (make-parameter #f))

(define-record example description behaviour pending meta spec-file)

(define (create-example description behaviour #!key (pending #f) (meta (list)) (context #f))
  (make-example  description behaviour pending  meta context))

(define-record-printer (example ex out)
  (fprintf out "#<example '~A' meta(~A)>" (example-description ex) (example-meta ex)))

(define-syntax it
  (syntax-rules (meta should not)
    ((_ should matcher)
     (add-example-to-context
      (create-example (sprintf "should ~S" (quote matcher))
                      (lambda ()
                        (expect (subject) matcher)))))
    ((_ (matcher arguments ...))
     (add-example-to-context
      (create-example (sprintf "~S" (quote (matcher arguments ...)))
                      (lambda ()
                        (expect (subject) (matcher arguments ...))))))
    ((_ description)
     (add-example-to-context (create-example description #f pending: #t)))
    ((_ description (meta (tag value ...) ...))
     (add-example-to-context (create-example description #f pending: #t meta: '((tag value ...) ...))))
    ((_ description (meta (tag value ...) ...) code more-code ...)
     (add-example-to-context (create-example description
                                             (lambda () code more-code ...)
                                             meta: '((tag value ...) ...))))
    ((_ description code more-code ...)
     (add-example-to-context (create-example description
                                             (lambda () code more-code ...))))))

(define (add-example-to-context example #!optional (context (current-context)))
  (returning example
    (when context
      (example-spec-file-set! example (current-spec-file))
      (context-examples-set! context (cons example (context-examples context))))))

(define (format-condition-properties exn without)
  (let* ((cps (condition->list exn))
         (eps (remove (lambda (x) (memq (car x) without))
                      (or (alist-ref 'exn cps) '())))
         (cps (alist-update! 'exn eps cps)))
    (with-output-to-string
        (lambda ()
          (for-each 
           (lambda (cp)
             (printf "~A:~%" (car cp))
             (for-each (lambda (p)
                         (printf "  ~A: ~S~%" (car p) (cadr p)))
                       (cdr cp)))
           cps)))))

(define (run-example example)
  (let((behaviour (example-behaviour example))
       (result    (make-example-result 'succeeded '() example (example-spec-file example))))
    (parameterize ((current-example-result result))
      (cond
       ((example-pending example)
        (example-result-status-set! result 'pending))
       (else
        (call-with-current-continuation
         (lambda (exit)
           (handle-exceptions exn
               (begin
                 ;(signal exn)
                 (fail-current-example-with! (sprintf "Error: ~S~%~A"
                                                      ((condition-property-accessor 'exn 'message) exn)
                                                      (format-condition-properties exn '(message call-chain)))))
             (call-with-exit-handler behaviour (make-exit-handler exit result)))))))
       result)))

(define (call-with-exit-handler code handler)
  (parameterize ((current-exit-continuation handler))
    (code)))

(define (make-exit-handler exit result)
  (lambda (status)
    (example-result-status-set! result status)
    (exit #f)))

(define (pending)
  (when (current-exit-continuation)
    ((current-exit-continuation) 'pending)))

(define (fail-current-example-with! message)
  (let ((result (current-example-result)))
    (when result
      (example-result-status-set! result 'failed)
      (add-failure-to-example-result result message)
      (when (current-exit-continuation)
        ((current-exit-continuation) 'failed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example-Result
;;
;; This is the result of an example run.
;; It holds statistics about the example run that is used by the
;; reporting-module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record example-result status messages example spec-file)

(define current-example-result (make-parameter #f))

(define (example-failed? result)
  ;(printf "~A~%" (example-result-messages result))
  (eq? 'failed (example-result-status result)))

(define (example-pending? result)
  (eq? 'pending (example-result-status result)))

(define (add-failure-to-example-result result message)
  (let ((messages (example-result-messages result)))
    (example-result-messages-set! result (append messages message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expectation
;;
;; Expectations are the heart of your descriptions
;; they allow you to formulate assertions that make up your example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define negative-expectation (make-parameter #f))
(define-for-syntax current-shorthand-matcher (make-parameter #f))

(define-syntax expect
  (syntax-rules ()
    ((_ form)
     (run-expectation
      (quote form)
      (delay #f)
      (make-matcher
       (lambda (_) form)
       (lambda (_ subject negate)
         (if negate
             (sprintf "Expected ~S not to evaluate to true but it did" (quote form))
             (sprintf "Expected ~S to be true but was false" (quote form)))))
      (negative-expectation)))
    ((_ subject matcher)
     (run-expectation
      (quote subject)
      (delay subject) matcher
      (negative-expectation)))))

(define (run-expectation form subject matcher #!optional (negate #f))
  (let ((check (matcher-check matcher)))
    (cond
     (negate
      (when (check subject)
        (fail-current-example-with! (generate-failure-message matcher form subject #t))))
     (else
      (unless (check subject)
        (fail-current-example-with! (generate-failure-message matcher form subject #f)))))))

;;
;; TODO finish this so that we can set the short-hand later
;; sth like
;; (define-for-syntax +default-shorthand-matcher+ #f)
;; (define-syntax expect
;;   (lambda (form rename env)
;;     (let ((%run-expectation (rename 'run-expectation))
;;           (%delay (rename 'delay))
;;           (%make-matcher (rename 'make-matcher))
;;           (%quote (rename 'quote))
;;           (%negative-expectation (rename 'negative-expectation)))
;;       (match form
;;         (
;;          (_ subject => a0 a1 ...)
;;           (if +default-shorthand-matcher+
;;           `(,%expect ,subject (,+default-shorthand-matcher+ a0 a1 ...))
;;            (syntax-error "You need to set the +default-shorthand-matcher+" )))
;;          (_ frm)
;;          `(,%run-expectation
;;            (,%quote ,frm)
;;            (,%delay #f)
;;            (,%make-matcher
;;             (lambda (s) ,frm)
;;             (lambda
;;              (f subject negate)
;;              (if negate
;;                  (sprintf "Expected ~S not to evaluate to true but it did" (,%quote ,frm))
;;                  (sprintf "Expected ~S to be true but was false" (,%quote ,frm)))))))
;;         ((_ subject matcher)
;;          `(,%run-expectation (,%quote subject) (,%delay subject) ,matcher (,%negative-expectation)))))))





(define-syntax do-not
  (syntax-rules ()
    ((_ expectation)
     (parameterize ((negative-expectation #t))
       expectation))))

(define-syntax to
  (syntax-rules ()
    ((_ form)
     form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matcher
;; 
;; A matcher is responsible to check if a certain behaviour
;; is present. It's a check agains the existing behaviour.
;; Matchers do generate failure-messages if the expectation is not met
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record matcher check failure-message-generator)

(define (generate-failure-message matcher form subject #!optional (negate #f))
  (let ((message-generator (matcher-failure-message-generator matcher)))
    (message-generator form subject negate)))

(define-syntax matcher
  (syntax-rules (check message)
    ((_ (check (subject) code more-code ...)
        (message (form message-subject negate) msg-code more-msg-code ...))
     (make-matcher
      (lambda (subject) code more-code ...)
      (lambda (form message-subject negate) msg-code more-msg-code ...)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;
;; This is a simple mechanism to maintain state between examples. It
;; can be used in situations where a simple let over the examples
;; doesn't suffice. 
;; This will most likely be used in setup/teardown code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *state* (make-hash-table))

(define ($ variable #!key (default #f))
  (hash-table-ref/default *state* variable default))

(define (reset-state!)
  (hash-table-clear! *state*))

(define (set-state! key value)
  (hash-table-set! *state* key value))

(set! (setter $) set-state!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reporting
;; 
;; At some point we need to notify the programmer about the state of her expectations.
;; The reporter is responsible for presenting the result to the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-standard-reporter #!key (report-port (current-output-port)))
  (let ((failed 0)
        (successful 0)
        (pending 0))
    (lambda (data #!key (mode 'adhoc))
      (cond
       ((eq? mode 'summary)
        (fprintf report-port "~%~%Total: ~A Successful: ~A Failed: ~A Pending: ~A ~%"
                 (+ failed successful pending) successful failed pending)
        (= failed 0))
       ((context? data) #t)
       ((example? data) #t)
       ((example-result? data)
        (let ((example (example-result-example data)))
          (cond
           ((example-failed? data)
            (set! failed (+ 1 failed))
            (fprintf report-port "Failure: ~A~%" (example-result-messages data)))
           ((example-pending? data)
            (set! pending (+ 1 pending))
            (fprintf report-port "Pending: ~A~%"  (example-description example)))
           (else
            (set! successful (+ 1 successful))
            (fprintf report-port "Success: ~A~%" (example-description example))))
          (flush-output report-port)))))))

)
