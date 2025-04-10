import numpy as np


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


if __name__ == "__main__":
    print(read_data("places.txt"))
