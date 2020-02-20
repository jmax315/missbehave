Feature: Selectively run specs
  In order to run only certain tests
  As a developer using the behave cli
  I want to be able to filter examples and contexts by tags

Background: 
  Given there is a feature file "tags.scm" with:
  """
  (describe "meta on context"
    (meta (somegroup))
    (subject-set! 42)
    (it should (be a number))
    (it should (be > 0)))

  (describe "failures"
   (it "should fail"
     (meta (fail))
     (expect 0 (be 1))))

  (describe "successful1"
   (it "should succeed"
     (meta (succeed))
     (expect 0 (be 0))))

  (describe "more than one tag"
    (it "should be running"
      (meta (bread butter))
      (expect 0 (be 0))))

   (describe "tags with values"
     (it "should be running with value"
       (meta ((bread . real)))
       (expect 0 (be 0))))
     
  """

@no-clobber @announce
Scenario: Tags on contexts
  When I run `../../behave --nocolor --tags @somegroup tags.scm`
  Then the exit status should be 0
  And the output should contain:
  """
  Total: 2 Successful: 2 Pending: 0 Failures: 0
  """

@no-clobber @announce
Scenario: Tags on examples 
  When I run `../../behave --nocolor --tags @fail tags.scm`
  Then the exit status should be 2
  And the output should contain:
  """
  Total: 1 Successful: 0 Pending: 0 Failures: 1
  """

@no-clobber @announce
Scenario: Negative tags
  When I run `../../behave --nocolor --tags ~@fail tags.scm`
  Then the exit status should be 0
  And the output should contain:
  """
  Total: 5 Successful: 5 Pending: 0 Failures: 0
  """


