from unittest import TestCase

from segregation_system.balancing_report import BalancingReport
from segregation_system.test.test_utility_lib import generate_random_prepared_sessions_object_list


class TestBalancingReport(TestCase):
    def setUp(self):
        self.prepared_sessions = generate_random_prepared_sessions_object_list(20)

    def test_balancing_report(self):
        # Create BalancingReport from prepared sessions
        bal = BalancingReport(self.prepared_sessions)

        # Count labels manually
        expected_counts = {"move": 0, "turn_left": 0, "turn_right": 0}
        for session in self.prepared_sessions:
            expected_counts[session.label] += 1

        # Verify that the counts match the expected values
        self.assertEqual(bal.move, expected_counts["move"], "The count for 'move' is incorrect.")
        self.assertEqual(bal.turn_left, expected_counts["turnLeft"], "The count for 'turn_left' is incorrect.")
        self.assertEqual(bal.turn_right, expected_counts["turnRight"], "The count for 'turn_right' is incorrect.")

        print("Test passed. Balancing report matches expected values.")
