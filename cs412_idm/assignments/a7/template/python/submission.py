# Submit this file to Gradescope
import math
from typing import Dict, List, Tuple
# You may use any built-in standard Python libraries
# You may NOT use any non-standard Python libraries such as numpy, scikit-learn, etc.

num_C = 7 # Represents the total number of classes

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
    return None

  def label(self, X_train: List[List[int]], Y_train: List[int], X_test: List[List[int]]) -> List[int]:
    """Calculate the classification labels for each test datapoint
    Args:
      X_train: Row i represents the i-th training datapoint
      Y_train: The i-th integer represents the class label for the i-th training datapoint
      X_test: Row i represents the i-th testing datapoint
    Returns:
      A list of length M where M is the number of datapoints in the test set
    """
    # implement this function
    return None
