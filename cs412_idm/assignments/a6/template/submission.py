from typing import List, Dict, Tuple, Set
import math


class Node:
    """
    This class, Node, represents a single node in a decision tree. It is designed to store information about the tree
    structure and the specific split criteria at each node. It is important to note that this class should NOT be
    modified as it is part of the assignment and will be used by the autograder.

    The attributes of the Node class are:
    - split_dim: The dimension/feature along which the node splits the data (-1 by default, indicating uninitialized)
    - split_point: The value used for splitting the data at this node (-1 by default, indicating uninitialized)
    - label: The class label assigned to this node, which is the majority label of the data at this node. If there is a tie,
    the numerically smaller label is assigned (-1 by default, indicating uninitialized)
    - left: The left child node of this node (None by default). Either None or a Node object.
    - right: The right child node of this node (None by default) Either None or a Node object.
    """

    def __init__(self):
        self.split_dim = -1
        self.split_point = -1
        self.label = -1
        self.left = None
        self.right = None


class Solution:
    """
    Example usage of the Node class to build a decision tree using a custom method called split_node():

    # In the fit method, create the root node and call the split_node() method to build the decision tree
    self.root = Node()
    self.split_node(self.root, data, ..., depth=0)

    def split_node(self, node, data, ..., depth):
      # Your implementation to calculate split_dim, split_point, and label for the given node and data
      # ...

      # Assign the calculated values to the node
      node.split_dim = split_dim
      node.split_point = split_point
      node.label = label

      # Recursively call split_node() for the left and right child nodes if the current node is not a leaf node
      # Remember, a leaf node is one that either only has data from one class or one that is at the maximum depth
      if not is_leaf:
          left_child = Node()
          right_child = Node()

          split_node(left_child, left_data, ..., depth+1)
          split_node(right_child, right_data, ..., depth+1)
    """

    def __init__(self):
        self.root = None
        self.max_depth = 2

    def _entropy(self, labels: List[int]) -> float:
        if not labels:
            return 0.0

        label_counts: Dict[int, int] = {}
        for label in labels:
            label_counts[label] = label_counts.get(label, 0) + 1

        entropy = 0.0
        total_count = len(labels)
        for label in label_counts:
            probability = label_counts[label] / total_count
            if probability > 0:
                entropy -= probability * math.log2(probability)
        return entropy

    def split_info(self, data: List[List[float]], labels: List[int], split_dim: int, split_point: float) -> float:
        """
        Compute the information needed to classify a dataset if it's split
        with the given splitting dimension and splitting point, i.e. Info_A in the slides.

        Parameters:
        data (List[List]): A nested list representing the dataset.
        label (List): A list containing the class labels for each data point.
        split_dim (int): The dimension/attribute index to split the data on.
        split_point (float): The value at which the data should be split along the given dimension.

        Returns:
        float: The calculated Info_A value for the given split. Do NOT round this value
        """
        if not data:
            return 0.0

        left_labels: List[int] = []
        right_labels: List[int] = []

        for i, point in enumerate(data):
            if split_dim < len(point):
                if point[split_dim] <= split_point:
                    left_labels.append(labels[i])
                else:
                    right_labels.append(labels[i])

        total_count = len(labels)
        if total_count == 0:
            return 0.0

        info_a = 0.0
        if left_labels:
            info_a += (len(left_labels) / total_count) * self._entropy(left_labels)
        if right_labels:
            info_a += (len(right_labels) / total_count) * self._entropy(right_labels)

        return info_a

    def _get_candidate_split_points(self, data: List[List[float]], dim: int) -> List[float]:
        if not data:
            return []

        values_in_dim = []

        for point in data:
            if dim < len(point):
                values_in_dim.append(point[dim])

        if not values_in_dim:
            return []

        all_values_in_dim_sorted = sorted(values_in_dim)

        candidate_splits = []

        if len(all_values_in_dim_sorted) > 1:
            for i in range(len(all_values_in_dim_sorted) - 1):
                midpoint = (all_values_in_dim_sorted[i] + all_values_in_dim_sorted[i+1]) / 2.0
                candidate_splits.append(midpoint)

        if not candidate_splits:
            return []
        else:
            return sorted(list(set(candidate_splits)))

    def _information_gain(self, data: List[List[float]], labels: List[int], split_dim: int, split_point: float) -> float:
        if not labels:
            return 0.0

        initial_entropy = self._entropy(labels)
        info_needed = self.split_info(data, labels, split_dim, split_point)
        gain = initial_entropy - info_needed
        return gain

    def _get_majority_label(self, labels: List[int]) -> int:
        if not labels:
            return -1

        label_counts: Dict[int, int] = {}
        for label in labels:
            label_counts[label] = label_counts.get(label, 0) + 1

        if not label_counts:
            return -1

        max_count = 0
        majority_label = -1

        for label in sorted(label_counts.keys()):
            if label_counts[label] > max_count:
                max_count = label_counts[label]
                majority_label = label

        return majority_label

    def _find_best_split(self, data: List[List[float]], labels: List[int]) -> Tuple[int, float, float]:
        best_gain = -1.0
        best_split_dim = -1
        best_split_point = -1.0
        num_features = 0

        if data:
            num_features = len(data[0])

        if num_features == 0:
            return -1, -1.0, -1.0

        current_entropy = self._entropy(labels)
        if current_entropy == 0:
            return  -1, -1.0, 0.0

        for dim in range(num_features):
            candidate_points = self._get_candidate_split_points(data, dim)
            for point in candidate_points:
                gain = self._information_gain(data, labels, dim, point)

                tolerance = 1e-9
                if gain > best_gain + tolerance:
                    best_gain = gain
                    best_split_dim = dim
                    best_split_point = point
                elif abs(gain - best_gain) < tolerance:
                    if dim < best_split_dim:
                        best_gain = gain
                        best_split_dim = dim
                        best_split_point = point

        if best_gain <= 0 + tolerance:
            return -1, -1.0, 0.0

        return best_split_dim, best_split_point, best_gain

    def _build_tree(self, node: Node, data: List[List[float]], labels: List[int], depth: int):
        node.label = self._get_majority_label(labels)

        unique_labels: Set[int] = set(labels)
        if depth >= self.max_depth or len(unique_labels) <= 1 or not data:
            node.split_dim = -1
            node.split_point = -1.0
            node.left = None
            node.right = None
            return

        split_dim, split_point, best_gain = self._find_best_split(data, labels)

        if split_dim == -1 or best_gain <= 0:
            node.split_dim = -1
            node.split_point = -1.0
            node.left = None
            node.right = None
            return

        node.split_dim = split_dim
        node.split_point = split_point

        left_data: List[List[float]] = []
        left_labels: List[int] = []
        right_data: List[List[float]] = []
        right_labels: List[int] = []

        for i, point in enumerate(data):
            if split_dim < len(point):
                if point[split_dim] <= split_point:
                    left_data.append(point)
                    left_labels.append(labels[i])
                else:
                    right_data.append(point)
                    right_labels.append(labels[i])

        if left_data:
            node.left = Node()
            self._build_tree(node.left, left_data, left_labels, depth + 1)
        else:
            node.left = Node()
            node.left.label = node.label
            node.left.split_dim = -1
            node.left.split_point = -1.0

        if right_data:
            node.right = Node()
            self._build_tree(node.right, right_data, right_labels, depth + 1)
        else:
            node.right = Node()
            node.right.label = node.label
            node.right.split_dim = -1
            node.right.split_point = -1.0

    def fit(self, train_data: List[List[float]], train_label: List[int]) -> None:
        """
        Fit the decision tree model using the provided training data and labels.

        Parameters:
        train_data (List[List[float]]): A nested list of floating point numbers representing the training data.
        train_label (List[int]): A list of integers representing the class labels for each data point in the training set.

        This method initializes the decision tree model by creating the root node. It then builds the decision tree starting 
        from the root node

        It is important to note that for tree structure evaluation, the autograder for this assignment
        first calls this method. It then performs tree traversals starting from the root node in order to check whether 
        the tree structure is correct. 

        So it is very important to ensure that self.root is assigned correctly to the root node

        It is best to use a different method (such as in the example above) to build the decision tree.
        """

        self.root = Node()
        if not train_data or not train_label:
            self.root.label = -1
            self.root.split_dim = -1
            self.root.split_point = -1.0
            return

        self._build_tree(self.root, train_data, train_label, depth=0)

    def _predict_one(self, node: Node, data_point: List[float]) -> int:
        current_node = node
        while current_node.split_dim != -1:
            split_dim = current_node.split_dim
            split_point = current_node.split_point

            if split_dim < len(data_point):
                if data_point[split_dim] <= split_point:
                    if current_node.left:
                        current_node = current_node.left
                    else:
                        break
                else:
                    if current_node.right:
                        current_node = current_node.right
                    else:
                        break
            else:
                break

        return current_node.label

    def classify(self, train_data: List[List[float]], train_label: List[int], test_data: List[List[float]]) -> List[int]:
        """
        Classify the test data using a decision tree model built from the provided training data and labels.
        This method first fits the decision tree model using the provided training data and labels by calling the
        'fit()' method.

        Parameters:
        train_data (List[List[float]]): A nested list of floating point numbers representing the training data.
        train_label (List[int]): A list of integers representing the class labels for each data point in the training set.
        test_data (List[List[float]]): A nested list of floating point numbers representing the test data.

        Returns:
        List[int]: A list of integer predictions, which are the label predictions for the test data after fitting
                   the train data and labels to a decision tree.
        """
        self.fit(train_data, train_label)

        if not self.root:
            return [-1] * len(test_data)

        predictions: List[int] = []
        for point in test_data:
            pred = self._predict_one(self.root, point)
            predictions.append(pred)

        return predictions


# if __name__ == "__main__":
#     print("Running basic runtime test...")
#
#     train_data = [
#         [1.0, 5.0], [1.5, 5.5], [0.5, 4.5], [2.0, 5.0], # Class 0 (low f0)
#         [1.2, 1.0],                                      # Class 0 (low f0, low f1)
#         [7.0, 1.0], [8.0, 0.5], [9.5, 1.5], [6.5, 0.8], # Class 1 (high f0)
#         [7.5, 8.0], [8.5, 9.0]                            # Class 1 (high f0, high f1)
#     ]
#     train_label = [
#         0, 0, 0, 0,
#         0,
#         1, 1, 1, 1,
#         1, 1
#     ]
#
#     # Test Data
#     test_data = [
#         [1.1, 5.2], # Expect 0
#         [8.5, 1.0], # Expect 1
#         [0.8, 0.8], # Expect 0
#         [7.2, 9.0], # Expect 1
#         [4.0, 4.0]  # Borderline case for f0 split
#     ]
#     print(f"Train data points: {len(train_data)}")
#     print(f"Test data points: {len(test_data)}")
#
#     solution = Solution()
#
#     try:
#         info = solution.split_info(train_data, train_label, split_dim=0, split_point=4.0)
#         print(f"Split Info (dim=0, point=4.0): {info}") 
#         info2 = solution.split_info(train_data, train_label, split_dim=1, split_point=3.0)
#         print(f"Split Info (dim=1, point=3.0): {info2}")
#     except Exception as e:
#         print(f"Error during split_info test: {e}")
#
#     print("\nAttempting classification...")
#     try:
#         predictions = solution.classify(train_data, train_label, test_data)
#         print(f"Predicted labels: {predictions}")
#
#         if isinstance(predictions, list) and len(predictions) == len(test_data):
#              print("Output format (list length) seems correct.")
#              all_ints = all(isinstance(p, int) for p in predictions)
#              if all_ints:
#                  print("Output format (all integers) seems correct.")
#              else:
#                  print("ERROR: Predictions list contains non-integers!")
#         else:
#             print(f"ERROR: Output format is incorrect! Expected list of length {len(test_data)}, got {type(predictions)} of length {len(predictions) if isinstance(predictions, list) else 'N/A'}")
#
#     except Exception as e:
#         print(f"\n---!!! RUNTIME ERROR DURING CLASSIFICATION !!!---")
#         import traceback
#         traceback.print_exc()
#         print(f"---------------------------------------------------")
#
#     print("\nBasic runtime test finished.")
