"""
Author: Giovanni Ligato
"""

import random

class UniqueRandomGenerator:
    def __init__(self, lower_bound, upper_bound):
        """
        Initializes the generator with a range.

        :param lower_bound: The lower bound of the range (inclusive).
        :param upper_bound: The upper bound of the range (inclusive).
        """
        if lower_bound > upper_bound:
            raise ValueError("Lower bound must be less than or equal to the upper bound.")

        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
        self.remaining_numbers = set(range(lower_bound, upper_bound + 1))

    def generate(self):
        """
        Generates a random number from the range without duplicates.

        :return: A unique random number.
        :raises ValueError: If all numbers in the range have been used.
        """
        if not self.remaining_numbers:
            raise ValueError("All numbers in the range have been used.")

        number = random.choice(list(self.remaining_numbers))
        self.remaining_numbers.remove(number)
        return number

    def reset(self):
        """
        Resets the generator, allowing numbers to be generated again.
        """
        self.remaining_numbers = set(range(self.lower_bound, self.upper_bound + 1))


if __name__ == "__main__":
    # Example usage:
    generator = UniqueRandomGenerator(1, 10)

    try:
        while True:
            print(generator.generate())
    except ValueError as e:
        print(e)

    # Reset the generator and start again
    generator.reset()
    print("Generator reset. New sequence:")
    try:
        while True:
            print(generator.generate())
    except ValueError as e:
        print(e)
