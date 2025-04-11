# Submit this file to Gradescope
from typing import Dict, List, Tuple
# you may use other Python standard libraries, but not data
# science libraries, such as numpy, scikit-learn, etc.

class Solution:

  def confusion_matrix(self, true_labels: List[int], pred_labels: List[int]) -> Dict[Tuple[int, int], int]:
    """Calculate the confusion matrix and return it as a sparse matrix in dictionary form.
    Args:
      true_labels: list of true labels
      pred_labels: list of predicted labels
    Returns:
      A dictionary of (true_label, pred_label): count
    """
    ... # implement this function

  def jaccard(self, true_labels: List[int], pred_labels: List[int]) -> float:
    """Calculate the Jaccard index.
    Args:
      true_labels: list of true cluster labels
      pred_labels: list of predicted cluster labels
    Returns:
      The Jaccard index. Do NOT round this value.
    """
    ... # implement this function

  def nmi(self, true_labels: List[int], pred_labels: List[int]) -> float:
    """Calculate the normalized mutual information.
    Args:
      true_labels: list of true cluster labels
      pred_labels: list of predicted cluster labels
    Returns:
      The normalized mutual information. Do NOT round this value.
    """
    ... # implement this function
