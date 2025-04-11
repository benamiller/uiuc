# Submit this file to Gradescope
from typing import List
# you may use other Python standard libraries, but not data
# science libraries, such as numpy, scikit-learn, etc.
import math


class Solution:
    def _euclidean_distance(self, p1: List[float], p2: List[float]) -> float:
        return math.sqrt(sum([(a - b) ** 2 for a, b in zip(p1, p2)]))

    def _calculate_cluster_distance(
            self,
            cluster_1_indices: List[int],
            cluster_2_indices: List[int],
            X: List[List[float]],
            link_type: str
            ):
        distances = []
        for i in cluster_1_indices:
            for j in cluster_2_indices:
                dist = self._euclidean_distance(X[i], X[j])
                distances.append(dist)

        if link_type == 'single':
            return min(distances)

        elif link_type == 'complete':
            return max(distances)

        elif link_type == 'average':
            return sum(distances) / len(distances)

        else:
            raise ValueError("Invalid link type")

    def _hclus(self,
               X: List[List[float]], K: int, link_type: str) -> List[int]:
        n = len(X)
        if n == 0:
            return []
        if K > n or K <= 0:
            raise ValueError("K should be from 1 to the number of data points")

        clusters = [[i] for i in range(n)]

        while (len(clusters) > K):
            min_dist = float('inf')
            merge_indices = (-1, -1)

            for i in range(len(clusters)):
                for j in range(i + 1, len(clusters)):
                    dist = self._calculate_cluster_distance(
                            clusters[i], clusters[j], X, link_type)
                    if dist < min_dist:
                        min_dist = dist
                        merge_indices = (i, j)

            if merge_indices == (-1, -1):
                break

            # We should definitely remove the bigger of the two first to not
            # shift the clusters array and then pop the wrong cluster
            # idx1 will be bigger, and should be popped first
            idx2, idx1 = sorted(merge_indices)
            # print(f"idx1 is {idx1}, and idx2 is {idx2}")
            merged_cluster = clusters[idx1] + clusters[idx2]
            clusters.pop(idx1)
            clusters.pop(idx2)
            clusters.append(merged_cluster)

        # The cluster containing point 0 might not have label 0, but per the
        # docstring in hclus_single_link, this is okay
        labels = [-1] * n
        current_label = 0
        for cluster_indices in clusters:
            for index in cluster_indices:
                labels[index] = current_label
            current_label += 1

        return labels

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
    # Should be ~100
    print(distance)

    cluster_1_indices = [0, 1]
    cluster_2_indices = [2, 3]
    single_link_distance = sol._calculate_cluster_distance(
            cluster_1_indices,
            cluster_2_indices,
            data,
            'single')

    # Should be 0 -> 3 at ~42.8
    print(single_link_distance)

    complete_link_distance = sol._calculate_cluster_distance(
            cluster_1_indices,
            cluster_2_indices,
            data,
            'complete')

    # Should be 1 -> 2 at ~247
    print(complete_link_distance)

    average_link_distance = sol._calculate_cluster_distance(
            cluster_1_indices,
            cluster_2_indices,
            data,
            'average')

    # 0 -> 2 = ~149.85
    # 0 -> 3 = ~42.8
    # 1 -> 2 = ~247.4
    # 1 -> 3 = ~104.68
    # Average of these is ~136
    print(average_link_distance)

    # This should be the same as output00.txt
    labels = sol._hclus(data, 2, 'single')
    print(labels)
