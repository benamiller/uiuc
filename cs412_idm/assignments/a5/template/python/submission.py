# Submit this file to Gradescope
from typing import Dict, List, Tuple
# you may use other Python standard libraries, but not data
# science libraries, such as numpy, scikit-learn, etc.
from collections import Counter, defaultdict
import math


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
        n = len(true_labels)
        if n == 0:
            return 1.0

        tp = 0
        fp = 0
        fn = 0

        for i in range(n):
            for j in range(i + 1, n):
                same_cluster_label = (true_labels[i] == true_labels[j])
                same_cluster_pred = (pred_labels[i] == pred_labels[j])

                if same_cluster_label and same_cluster_pred:
                    tp += 1
                elif same_cluster_label and not same_cluster_pred:
                    fn += 1
                elif not same_cluster_label and same_cluster_pred:
                    fp += 1

        denominator = tp + fp + fn

        # Avoid division by zero, just 1.0 if we had no true positive as all
        if denominator == 0:
            return 1.0

        return float(tp) / denominator

    def nmi(self, true_labels: List[int], pred_labels: List[int]) -> float:
        """Calculate the normalized mutual information.
        Args:
            true_labels: list of true cluster labels
            pred_labels: list of predicted cluster labels
        Returns:
            The normalized mutual information. Do NOT round this value.
        """
        n = len(true_labels)
        if n == 0:
            return 1.0

        true_counts = Counter(true_labels)
        pred_counts = Counter(pred_labels)

        entropy_true = 0.0
        for label in true_counts:
            prob_true = true_counts[label] / n
            entropy_true -= prob_true * math.log2(prob_true)

        entropy_pred = 0.0
        for pred in pred_counts:
            prob_pred = pred_counts[pred] / n
            entropy_pred -= prob_pred * math.log2(prob_pred)

        mutual_information = 0.0
        conf_matrix = self.confusion_matrix(true_labels, pred_labels)

        for (true_label, pred_label), count in conf_matrix.items():
            joint_probability = count / n
            prob_true = true_counts[true_label] / n
            prob_pred = pred_counts[pred_label] / n

            if joint_probability > 0:
                mutual_information += (
                        joint_probability *
                        math.log2(joint_probability / (prob_true * prob_pred)))

        denominator = math.sqrt(entropy_true * entropy_pred)

        if denominator == 0:
            return 1.0 if mutual_information == 0 else 0.0

        return mutual_information / denominator


if __name__ == "__main__":
    sol = Solution()
    true_labels = [0, 1, 0, 0, 1, 1, 1, 0, 1, 1]
    pred_labels = [1, 0, 1, 1, 0, 1, 1, 1, 0, 1]

    conf_matrix = sol.confusion_matrix(true_labels, pred_labels)
    print(conf_matrix)

    # New labels and preds to match input01.txt with Jaccard output
    true_labels = [0, 0, 1, 0, 1, 0, 0, 1, 0, 1]
    pred_labels = [1, 1, 0, 1, 1, 0, 1, 0, 0, 0]
    jaccard = sol.jaccard(true_labels, pred_labels)
    # Should be ~0.3226
    print(jaccard)

    true_labels = [0, 0, 0, 1, 1, 0, 1, 0, 1, 0]
    pred_labels = [1, 1, 1, 1, 0, 0, 1, 0, 0, 1]
    normalized_mutual_information = sol.nmi(true_labels, pred_labels)
    # Should be ~0.0206
    print(normalized_mutual_information)

