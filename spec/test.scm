;; 
;; %%HEADER%%
;; 

;(load "./missbehave.scm")
;(import missbehave)
(use missbehave missbehave-matchers missbehave-stubs miscmacros)

(define (callee . args) 'called)
(define (call-it n)
  (repeat n (callee)))

(describe "Missbehave features"
  (describe "implicit subject"
     (subject-set! 42)
     (it (is > 0))
     (it (is a number))
     (it should (be 42)))
  
         
  (describe "Simple be matchers"
     (it "must be true"
         (expect #t (be true)))
     (it "must be a string"
         (expect "hello" (be a string)))
     (it "can use standard operators"
         (expect 3 (be > 0)))
     (it "can use predicates"
         (expect '() (be null?))))


  (define (numbers ls) ls)
  
  (describe "Have"
    (it "can be used with a collection procedure"
      (expect '(1 2 3) (has 3 numbers)))

    (it "supports arbritary sugar"
      (expect "foo" (has 3 characters))))


  (describe "Matches string"
    (it "checks if regex matches string"
      (expect '(: (+ digit)) (matches-string "1234")))
    (it "checks the submatches"
      (expect '(: (+ digit) (submatch (+ any))) (matches-string "1234foo" with-matches: '((1 . "foo"))))))

  ;; (describe "Match subject"
  ;;   (it "checks if subject matches regex"
  ;;     (expect "1234" (matches '(: (+ digit))))))

  

  (describe "Pending"
     (it "is implicitly pending")
     (it "is explicitly pending"
         (pending)
         (expect '() (be a number))))         

  (describe "Procedure expectations"
    (context "Application-count"
      (it "checks application count"
          (expect (call-it 1) (call callee once)))
      (it "checks application count > 1"
          (expect (call-it 4) (call callee (4 times))))
      (it "checks for no calls"
          (expect (+ 1 1)  (call callee never))))
    (context "Arguments"
     (it "checks arguments"
         (expect (callee 1 2) (call callee (with 1 2))))
     (it "mixes arguments and application count"
         (expect (begin (callee 1 2) (callee 1 2)) (call callee (with 1 2) twice)))))


  (describe "Procedure stubs"
    (it "can stub return values"
        (stub! callee (returns 'not-called))
        (expect (callee)  (be 'not-called)))

    (it "provides temporary stubs"
        (let ((proc (lambda () 'test)))
          (expect (proc)  (be 'test))
          (with-stubs! ((proc (returns 'passed)))
            (expect (proc)  (be 'passed)))
          (expect (proc)  (be 'test))))))
