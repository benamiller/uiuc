# Submit this file to Gradescope
import math
from typing import Dict, List
# You may use any built-in standard Python libraries
# You may NOT use any non-standard Python libraries such as numpy, scikit-learn, etc.

num_C = 7
alpha = 0.1

num_unique_feature_values = [
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 6, 2, 2, 2,
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
        priors = self.prior(X_train, Y_train)
        log_priors: Dict[int, float] = {}
        for c in range(1, num_C + 1):
            if priors[c-1] > 0:
                log_priors[c] = math.log(priors[c-1])
            else:
                log_priors[c] = -float('inf')

        N = len(Y_train)
        class_counts: Dict[int, int] = {c: 0 for c in range(1, num_C + 1)}
        feature_class_counts: List[Dict[int, Dict[int, int]]] = []

        for i in range(num_features):
            feature_class_counts.append({})

        for i in range(N):
            label = Y_train[i]
            if 1 <= label <= num_C:
                class_counts[label] += 1
                features = X_train[i]

                if len(features) == num_features:
                    for j in range(num_features):
                        feature_value = features[j]

                        if feature_value not in feature_class_counts[j]:
                            feature_class_counts[j][feature_value] = {c: 0 for c in range(1, num_C + 1)}
                        elif label not in feature_class_counts[j][feature_value]:
                            feature_class_counts[j][feature_value][label] = 0

                        feature_class_counts[j][feature_value][label] += 1

        predictions = []
        for test_point in X_test:
            if len(test_point) != num_features:
                predictions.append(1)
                continue

            log_posteriors: Dict[int, float] = {}
            for c in range(1, num_C + 1):
                log_posteriors[c] = log_priors[c]

                for i in range(num_features):
                    feature_value = test_point[i]
                    num_unique = num_unique_feature_values[i]

                    count = 0
                    if feature_value in feature_class_counts[i] and c in feature_class_counts[i][feature_value]:
                        count = feature_class_counts[i][feature_value][c]

                    numerator = count + alpha
                    denominator = class_counts[c] + alpha * num_unique

                    if numerator > 0 and denominator > 0:
                        log_posteriors[c] += math.log(numerator / denominator)
                    else:
                        log_posteriors[c] += -float('inf')

            best_class = -1
            max_log_posterior = -float('inf')

            for c in range(1, num_C + 1):
                if log_posteriors[c] > max_log_posterior:
                    max_log_posterior = log_posteriors[c]
                    best_class = c

            if best_class == -1:
                best_class = 1

            predictions.append(best_class)

        return predictions

# if __name__ == "__main__":
#     print("Running Naive Bayes basic runtime test...")
#
#     # Class 1: Mammal-like
#     x_train_1 = [1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 4, 1, 0, 1] 
#     # Class 2: Bird-like
#     x_train_2 = [0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0] 
#     # Class 4: Fish-like
#     x_train_3 = [0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0] 
#     # Class 7: Invertebrate-like
#     x_train_4 = [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0] 
#     # Another Class 1
#     x_train_5 = [1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 4, 1, 1, 1]
#     # Class 6: Bug-like
#     x_train_6 = [1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 6, 0, 0, 0]
#
#     X_train_sample = [x_train_1, x_train_2, x_train_3, x_train_4, x_train_5, x_train_6]
#     Y_train_sample = [1, 2, 4, 7, 1, 6] 
#
#     # Should be like mammal (Class 1)
#     x_test_1 = [1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 4, 1, 0, 0] 
#     # Should be like fish (Class 4) / invertebrate (Class 7)?
#     x_test_2 = [0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0] 
#     # Should be like bird (Class 2)
#     x_test_3 = [0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 2, 1, 0, 0]
#     # Should be like bug (Class 6)
#     x_test_4 = [0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 6, 0, 0, 0]
#
#     X_test_sample = [x_test_1, x_test_2, x_test_3, x_test_4]
#
#     print(f"Sample Train data points: {len(X_train_sample)}")
#     print(f"Sample Test data points: {len(X_test_sample)}")
#     print(f"Number of features: {len(X_train_sample[0]) if X_train_sample else 'N/A'}")
#
#     solution = Solution()
#
#     print("\nTesting prior calculation...")
#     try:
#         prior_probs = solution.prior(X_train_sample, Y_train_sample)
#         print(f"Calculated priors: {prior_probs}")
#         if prior_probs and len(prior_probs) == 7:
#             print(f"Sum of priors: {sum(prior_probs):.4f}")
#             print("Prior calculation executed.")
#         else:
#             print(f"ERROR: Prior calculation returned unexpected result type/length: {type(prior_probs)}")
#
#     except Exception as e:
#         print(f"\n---!!! RUNTIME ERROR DURING prior() !!!---")
#         import traceback
#         traceback.print_exc()
#         print(f"-------------------------------------------")
#
#     print("\nTesting label prediction...")
#     try:
#         predicted_labels = solution.label(X_train_sample, Y_train_sample, X_test_sample)
#         print(f"Predicted labels: {predicted_labels}")
#
#         if isinstance(predicted_labels, list) and len(predicted_labels) == len(X_test_sample):
#             print("Output format (list length) seems correct.")
#             all_ints = all(isinstance(p, int) for p in predicted_labels)
#             if all_ints:
#                 print("Output format (all integers) seems correct.")
#                 all_in_range = all(1 <= p <= 7 for p in predicted_labels)
#                 if not all_in_range:
#                     print("WARNING: Some predicted labels are outside the expected range [1, 7]!")
#             else:
#                 print("ERROR: Predictions list contains non-integers!")
#         else:
#             print(f"ERROR: Output format is incorrect! Expected list of length {len(X_test_sample)}, got {type(predicted_labels)} of length {len(predicted_labels) if isinstance(predicted_labels, list) else 'N/A'}")
#
#     except Exception as e:
#         print(f"\n---!!! RUNTIME ERROR DURING label() !!!---")
#         import traceback
#         traceback.print_exc()
#         print(f"------------------------------------------")
#
#     print("\nBasic runtime test finished.")
#
