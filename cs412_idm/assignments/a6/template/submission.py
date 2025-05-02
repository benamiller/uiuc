from typing import List


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

    def split_info(self, data: List[List[float]], label: List[int], split_dim: int, split_point: float) -> float:
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

        values = sorted(list(set(point[dim] for point in data if dim < len(point))))

        split_points = []

        if len(values) > 1:
            candidate_splits = []
            for i in range(len(values) - 1):
                midpoint = (values[i] + values[i+1]) / 2.0
                candidate_splits.append(midpoint)

            split_points = sorted(list(set(candidate_splits)))

        return split_points

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

        label_counts = Dict[int, int] = {}
        for label in labels:
            label_counts = label_counts.get(label, 0) + 1

        if not label_counts:
            return -1

        max_count = 0
        majority_label = -1

        for label in sorted(label_counts.keys()):
            if label_counts[labels] > max_count:
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

        left_data = List[List[float]] = []
        left_labels: List[int] = []
        right_data = List[List[float]] = []
        right_labels = List[int] = []

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
            self._built_tree(node.left, left_data, left_labels, depth + 1)
        else:
            node.left = Node()
            node.left.label = node.label
            node.left.split_dim = -1
            node.left.split_point = -1.0

        if right_data:
            node.right = Node()
            self._built_tree(node.right, right_data, right_labels, depth + 1)
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

  """
  Students are encouraged to implement as many additional methods as they find helpful in completing
  the assignment. These methods can be implemented either as class methods of the Solution class or as
  global methods, depending on design preferences.

  For instance, one essential method that must be implemented is a method to build out the decision tree recursively.
  """
