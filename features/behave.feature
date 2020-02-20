Feature: Use the behave cli
  In order to use missbehave as a specification framework
  As a user of missbehave
  I want a cli that allows me to run my specs and see the results

Scenario: Run without arguments
  Given there are no feature files
  When I run `../../behave`
  Then the exit status should be 1
  And the output should contain:
  """
  behave [options ...] file ...
  """

Scenario: Show help
  Given there are no feature files
  When I run `../../behave -h`
  Then the exit status should be 0
  And the output should contain:
  """
  behave [options ...] file ...
  """
  And the output should contain:
  """
  --help
  """

@no-clobber
Scenario: Run a file with failures
  Given there is a feature file "failing.scm" with:
  """
  (describe "failures"
   (it "should fail"
     (expect 0 (be 1))))
  """
  When I run `../../behave --nocolor failing.scm`
  Then the exit status should be 2
  And the output should contain:
  """
  Total: 1 Successful: 0 Pending: 0 Failures: 1
  """

@no-clobber
Scenario: Run a file with only successful examples
  Given there is a feature file "successful.scm" with:
  """
  (describe "succeed"
   (it "should succeed"
     (expect 0 (be 0))))
  """
  When I run `../../behave --nocolor successful.scm`
  Then the exit status should be 0
  And the output should contain:
  """
  Total: 1 Successful: 1 Pending: 0 Failures: 0
  """

@no-clobber
Scenario: Run a file with only pending examples
  Given there is a feature file "pending.scm" with:
  """
  (describe "pending"
   (it "should pend"
     (pending)
     (expect 0 (be 0))))
  """
  When I run `../../behave --nocolor pending.scm`
  Then the exit status should be 0
  And the output should contain:
  """
  Total: 1 Successful: 0 Pending: 1 Failures: 0
  """
