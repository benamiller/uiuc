# Submit this file to Gradescope
from typing import Dict, List, Tuple
# you may use other Python standard libraries, but not data
# science libraries, such as numpy, scikit-learn, etc.
from collections import defaultdict


class Solution:
    def confusion_matrix(
            self,
            true_labels: List[int],
            pred_labels: List[int]) -> Dict[Tuple[int, int], int]:
        """Calculate the confusion matrix
            and return it as a sparse matrix in dictionary form.
        Args:
            true_labels: list of true labels
            pred_labels: list of predicted labels
        Returns:
            A dictionary of (true_label, pred_label): count
        """
        conf_matrix = defaultdict(int)
        for true_label, predicted_label in zip(true_labels, pred_labels):
            conf_matrix[(true_label, predicted_label)] += 1

        return dict(conf_matrix)

    def jaccard(self, true_labels: List[int], pred_labels: List[int]) -> float:
        """Calculate the Jaccard index.
        Args:
            true_labels: list of true cluster labels
            pred_labels: list of predicted cluster labels
        Returns:
            The Jaccard index. Do NOT round this value.
        """
        # TP / (TP + FP + FN)

    def nmi(self, true_labels: List[int], pred_labels: List[int]) -> float:
        """Calculate the normalized mutual information.
        Args:
            true_labels: list of true cluster labels
            pred_labels: list of predicted cluster labels
        Returns:
            The normalized mutual information. Do NOT round this value.
        """
        pass


if __name__ == "__main__":
    sol = Solution()
    true_labels = [0, 1, 0, 0, 1, 1, 1, 0, 1, 1]
    pred_labels = [1, 0, 1, 1, 0, 1, 1, 1, 0, 1]
    conf_matrix = sol.confusion_matrix(true_labels, pred_labels)
    print(conf_matrix)
