;; 
;; %%HEADER%%
;; 

(module missbehave-stubs

(with-stubs! stub! clear-stubs! returns)

(import chicken scheme)
(require-extension advice)


(define *current-stub-removers* (make-parameter (list)))

(define (stub! procedure stub)
  (let ((advice-id (advise 'around procedure (lambda (proc args) (apply stub args)))))
    (add-stub-remover! (lambda () (unadvise procedure advice-id)))))

(define (add-stub-remover! remover)
  (*current-stub-removers* (cons remover (*current-stub-removers*))))


(define (clear-stubs!)
  (for-each (lambda (remover) (remover)) (*current-stub-removers*))
  (*current-stub-removers* '()))

(define (returns argument . more-arguments)
  (lambda _
    (apply values argument more-arguments)))

(define-syntax with-stubs!
  (syntax-rules ()
    ((_ ((proc stub) ...) code more-code ...)
     (dynamic-wind
         (lambda ()
           (stub! proc stub)
           ...)
         (lambda () code more-code ...)
         (lambda () (clear-stubs!))))))

)
