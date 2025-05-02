# Submit this file to Gradescope
import math
from typing import Dict, List, Tuple
# You may use any built-in standard Python libraries
# You may NOT use any non-standard Python libraries such as numpy, scikit-learn, etc.


num_C = 7 # Represents the total number of classes
alpha = 0.1

num_unique_feature_values = [
    2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 6, 2, 2, 2,
]

num_features = len(num_unique_feature_values)


class Solution:
    def prior(self, X_train: List[List[int]], Y_train: List[int]) -> List[float]:
        """Calculate the prior probabilities of each class
        Args:
          X_train: Row i represents the i-th training datapoint
          Y_train: The i-th integer represents the class label for the i-th training datapoint
        Returns:
          A list of length num_C where num_C is the number of classes in the dataset
        """
        # implement this function
        N = len(Y_train)
        if N == 0:
            return [(0.0 + alpha) / (0.0 + alpha * num_C)] * num_C 

        class_counts: Dict[int, int] = {}
        for c in range(1, num_C + 1):
            class_counts[c] = 0

        for label in Y_train:
            if 1 <= label <= num_C:
                class_counts[label] += 1

        priors = []
        denominator = N + alpha * num_C

        for c in range(1, num_C + 1):
            numerator = class_counts[c] + alpha
            priors.append(numerator / denominator)

        return priors

    def label(self, X_train: List[List[int]], Y_train: List[int], X_test: List[List[int]]) -> List[int]:
        """Calculate the classification labels for each test datapoint
        Args:
          X_train: Row i represents the i-th training datapoint
          Y_train: The i-th integer represents the class label for the i-th training datapoint
          X_test: Row i represents the i-th testing datapoint
        Returns:
          A list of length M where M is the number of datapoints in the test set
        """
        return None

