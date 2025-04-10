import numpy as np
import random


INPUT_FILE = "places.txt"
OUTPUT_FILE = "clusters.txt"
K = 3
MAX_ITERATIONS = 100
CONVERGENCE_THRESHOLD = 1e-6


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


def kmeans(data, k, max_iterations, convergence_threshold):
    n, d = data.shape

    centroids = initialize_cluster_centroids(data, k)

    assignments = np.zeros(n, dtype=int)

    for iteration in range(max_iterations):
        print(f"~~~~~~~~ ITERATION {iteration + 1}/{max_iterations} ~~~~~~~~")
        old_centroids = np.copy(centroids)

        for i in range(n):
            point = data[i]
            distances = (
                [euclidean_distance(point, centroid) for centroid in centroids]
            )

            assignments[i] = np.argmin(distances)

        new_centroids = np.zeros((k, d))
        cluster_counts = np.zeros(k, dtype=int)

        for i in range(n):
            assigned_cluster_index = assignments[i]
            new_centroids[assigned_cluster_index] += data[i]
            cluster_counts[assigned_cluster_index] += 1

        for j in range(k):
            if cluster_counts[j] > 0:
                centroids[j] = new_centroids[j] / cluster_counts[j]
            else:
                # No points assigned to this cluster
                # For now, just re-initialize centroid
                print("Cluster with no data points. Re-initializing...")
                centroids[j] = data[random.choice(range(n))]

        centroid_position_delta = np.linalg.norm(centroids - old_centroids)
        print(f"Controid position change: {centroid_position_delta}")

        if (centroid_position_delta < convergence_threshold):
            print(f"Considered as converged after iteration {iteration + 1}")
            break

    else:
        print(f"Did not converge within {max_iterations + 1} iterations")

    return assignments, centroids


if __name__ == "__main__":
    data = read_data("places.txt")
    print(f"DATA:\n{data}\n")
    print(f"CLUSTERS:\n{initialize_cluster_centroids(data, 3)}")
    print(f"""ASSIGNMENTS:\n
          {kmeans(
              data,
              K,
              MAX_ITERATIONS,
              CONVERGENCE_THRESHOLD
              )
           [0]
           }
          """)
