import unittest

import testein


class TestSequenceFunctions(unittest.TestCase):

    def setUp(self):
        self.runner = testein.TestRunner(
            batch=True, debug_on_error=False, emacs='emacs',
            testfile='func-test.el', log_dir='log',
            ein_log_level=40, ein_message_level=30, ein_debug=False)

    def test_is_known_failure__yes(self):
        self.runner.stdout = """
ein: [info] Notebook Untitled0 is already opened.
ein: [verbose] ERT TESTING-GET-UNTITLED0-OR-CREATE end
   passed  7/7  ein:testing-get-untitled0-or-create

Ran 7 tests, 6 results as expected, 1 unexpected (2012-12-17 22:27:38+0000)

1 unexpected results:
   FAILED  ein:notebook-execute-current-cell-pyout-image

Wrote /home/travis/builds/....
        """
        assert self.runner.is_known_failure()

    def test_is_known_failure__no_failures(self):
        self.runner.stdout = """
ein: [info] Notebook Untitled0 is already opened.
ein: [verbose] ERT TESTING-GET-UNTITLED0-OR-CREATE end
   passed  7/7  ein:testing-get-untitled0-or-create

Ran 7 tests, 7 results as expected (2012-12-17 22:27:38+0000)

Wrote /home/travis/builds/....
        """
        assert self.runner.is_known_failure()

    def test_is_known_failure__no(self):
        self.runner.stdout = """
ein: [info] Notebook Untitled0 is already opened.
ein: [verbose] ERT TESTING-GET-UNTITLED0-OR-CREATE end
   passed  7/7  ein:testing-get-untitled0-or-create

Ran 7 tests, 4 results as expected, 2 unexpected (2012-12-17 22:27:38+0000)

2 unexpected results:
   FAILED  ein:notebook-execute-current-cell-pyout-image
   FAILED  some-unknown-failure

Wrote /home/travis/builds/....
        """
        assert not self.runner.is_known_failure()
