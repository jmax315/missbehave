;; 
;; %%HEADER%%
;; 

(describe "Failures in missbehave"
          (it "failed because of (error)"
              (error "Boom"))
          (it "failes because of an unsatisfied expectation"
              (expect #f to (be true))))
