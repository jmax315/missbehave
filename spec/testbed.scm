;; 
;; %%HEADER%%
;; 


;;implicit contexts

(it "should be valid to start an it without a surrounding context"
  (expect (eq? 2 3)))

;;implicit examples
(expect (eq? 5 5))

;;grouped expectations
(it "should be possible to group expectations "
  (expect 
      ((some-code)  (be true))
       (some-other   (be false))))

;;tabular data
(it "should be possible to provide tabular data"
  (for-every item in (list 1 2 2 4)
    (expect (some-code item) (be true))))

;;shared contexts
(it "should be possible to have shared contexts"
  (context "Some context" (shared "optional name")
     (it "should pass and pass and pass"))

  (context "Some other"
    (it should behave-like "optional name")))


;; message-modifiers
(expect (eq? 2 3) message: "Well other message")

;;implicit matcher
;; (describe "Something" (meta ((reality . #t)))
;;   (unfinished ((dictionary-set! (returns nothing))))
;;
;;   (it "Should do this and that"
;;     (expect (dictionary-set! 'dict 1) => 1)
;; Nice about the => is that it stands out and can be used to align
;; all expected results equally

(expect (eq? 2 3) (be 3))
(expect (eq? 2 3) => 2)


