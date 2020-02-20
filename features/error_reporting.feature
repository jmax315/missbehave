@errors
Feature: Report errors
  In order to see the errors even in long output
  As a developer using missbehave
  I want to see summary of errors right at the end of the output

@no-clobber @announce
Scenario: Report the file where the error accured and the error
  Given a file named "some-failures.scm" with:
  """
  (describe "a failure"
    (it "should fail"
     (expect #f (be true))))
  """
  When I run `behave --nocolor some-failures.scm`
  Then the exit status should be 2
  And the output should contain:
  """
  1) in some-failures.scm
  [F][1] It should fail
  Expected #f to be #t
  """

@no-clobber @announce
Scenario: Report more than one error
  Given a file named "some-failures.scm" with:
  """
  (describe "a failure"
    (it "should fail"
     (expect #f (be true)))
    (it "should fail too"
     (expect #f)))
  """
  When I run `behave --nocolor some-failures.scm`
  Then the exit status should be 2
  And the output should contain:
  """
  1) in some-failures.scm
  [F][1] It should fail
  Expected #f to be #t

  2) in some-failures.scm
  [F][2] It should fail too
  Expected #f to be true but was false
  """
