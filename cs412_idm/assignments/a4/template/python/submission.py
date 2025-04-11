# Submit this file to Gradescope
from typing import List
# you may use other Python standard libraries, but not data
# science libraries, such as numpy, scikit-learn, etc.
import math


class Solution:
    def _euclidean_distance(self, p1: List[float], p2: List[float]):
        return math.sqrt(sum([(a - b) ** 2 for a, b in zip(p1, p2)]))

    def hclus_single_link(self, X: List[List[float]], K: int) -> List[int]:
        """Single link hierarchical clustering
        Args:
          - X: 2D input data
          - K: the number of output clusters
        Returns:
          A list of integers (from 0 to K - 1) that represent class labels.
          The number does not matter as long as the clusters are correct.
          For example: [0, 0, 1] is treated the same as [1, 1, 0]"""
        # implement this function

    def hclus_average_link(self, X: List[List[float]], K: int) -> List[int]:
        """Average link hierarchical clustering"""
        # implement this function
        pass

    def hclus_complete_link(self, X: List[List[float]], K: int) -> List[int]:
        """Complete link hierarchical clustering"""
        # implement this function
        pass


if __name__ == "__main__":
    sol = Solution()
    data = [
            [7.8122, 7.0391],
            [-90.3764, 14.7628],
            [152.8991, -30.4529],
            [8.2569, 49.8364],
            [145.4259, -37.8743],
            ]
    distance = sol._euclidean_distance(data[0], data[1])
    print(distance)
