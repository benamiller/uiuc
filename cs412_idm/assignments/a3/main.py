import numpy as np
import random


def read_data(filename):
    points = []
    try:
        with open(filename, 'r') as f:
            for line in f:
                try:
                    lon_str, lat_str = line.strip().split(',')
                    points.append([float(lon_str), float(lat_str)])
                except ValueError:
                    print(f"Sus value, skipping line: {line.strip()}")
    except FileNotFoundError:
        print(f"File not found, {filename}")
        raise
    except Exception as e:
        print(f"Error occurred reading {filename}, {e}")
        raise

    return np.array(points)


def euclidean_distance(point1, point2):
    return np.linalg.norm(point1 - point2)


def initialize_cluster_centroids(data, k):
    n = data.shape[0]
    if k > n:
        raise ValueError("Got more centroids than points; Probably not right")

    # To prevent craziness, initialize clusters at actual points
    random_indices = random.sample(range(n), k)
    centroids = data[random_indices]

    return centroids


if __name__ == "__main__":
    data = read_data("places.txt")
    print(f"DATA:\n{data}\n")
    print(f"CLUSTERS:\n{initialize_cluster_centroids(data, 3)}")
