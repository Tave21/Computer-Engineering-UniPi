import random
import uuid
from typing import List

from segregation_system.prepared_session import PreparedSession


def generate_random_prepared_sessions_object_list(n: int) -> List[PreparedSession]:
    """
    Generates a list of randomized PreparedSession objects.

    Each PreparedSession contains:
    - A unique identifier (UUID).
    - Four randomly generated numerical features.
    - An activity selected randomly from a predefined list.
    - An environment selected randomly from a predefined list.
    - A label chosen randomly from a set of predefined labels.

    Args:
        n (int): The number of PreparedSession objects to generate.

    Returns:
        List[PreparedSession]: A list containing n randomized PreparedSession objects.
    """
    randomized_prepared_sessions = [
        PreparedSession(
            uuid=str(uuid.uuid4()),  # Generate a random UUID for the session.
            features=[
                random.uniform(-27, 28),  # Random feature value for the alpha band.
                random.uniform(-27, 28),  # Random feature value for the beta band.
                random.uniform(-27, 28),  # Random feature value for the theta band.
                random.uniform(-27, 28),  # Random feature value for the delta band.
                random.choice(["gaming", "shopping", "sport", "relax"]),  # Random activity.
                random.choice(["plain", "slippery", "slope", "house", "track"]),  # Random environment.
            ],
            label=random.choice(["move", "turnLeft", "turnRight"])  # Random label.
        )
        for _ in range(n)
    ]
    return randomized_prepared_sessions

def generate_random_prepared_session_object() -> PreparedSession:
    """
    Generates a single randomized PreparedSession object.

    This function internally uses `generate_random_prepared_sessions_object_list`
    to create a list with one element and returns the first item in the list.

    The PreparedSession contains:
    - A unique identifier (UUID).
    - Four randomly generated numerical features.
    - An activity selected randomly from a predefined list.
    - An environment selected randomly from a predefined list.
    - A label chosen randomly from a set of predefined labels.

    Returns:
        PreparedSession: A single randomized PreparedSession object.
    """
    # Use the helper function to generate a list with one PreparedSession.
    session = generate_random_prepared_sessions_object_list(1)
    return session[0]  # Return the first (and only) element in the list.


# Example to test the class
if __name__ == "__main__":
    sess = generate_random_prepared_session_object()