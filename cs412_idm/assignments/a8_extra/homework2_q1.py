import random
import math
import numpy as np
from xgboost import XGBClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn import datasets

# Do not import any other libraries

def get_model(method):
    model = None
    # Implement your code to return the appropriate model with the specified parameters here
    # Do NOT change the return statement
    model = None
    random_seed = 42

    if method == "DecisionTreeClassifier":
        model = DecisionTreeClassifier(max_depth=10, random_state=random_seed)
    elif method == "GaussianNB":
        model = GaussianNB()
    elif method == "LogisticRegression":
        model = LogisticRegression(penalty='l2', solver='lbfgs', random_state=random_seed, multi_class='multinomial')
    elif method == "RandomForestClassifier":
        model = RandomForestClassifier(max_depth=15, n_estimators=250, random_state=random_seed)
    elif method == "XGBClassifier":
        model = XGBClassifier(max_depth=7, random_state=random_seed)
    else:
        raise ValueError(f"Unknown method: {method}")

    return model

def get_splits(n, k, seed):
    splits = None
    # Implement your code to construct the splits here
    # Do NOT change the return statement
    splits = []
    indices = list(range(n))

    random.seed(seed)
    random.shuffle(indices)

    base_fold_size = n // k
    num_larger_folds = n % k

    start = 0
    for i in range(k):
        current_fold_size = base_fold_size + (1 if i < num_larger_folds else 0)

        fold_indices = indices[start: start + current_fold_size]
        splits.append(fold_indices)

        start += current_fold_size

    return splits

def my_cross_val(method, X, y, splits):
    errors = []
    # Implement your code to construct the list of errors here
    # Do NOT change the return statement

    k = len(splits)
    n = len(y)

    X = np.asarray(X)
    y = np.asarray(y)

    for i in range(k):
        test_indices_set = set(splits[i])
        test_indices = splits[i]

        train_indices = [idx for idx in range(n) if idx not in test_indices_set]

        X_train = X[train_indices]
        y_train = y[train_indices]
        X_test = X[test_indices]
        y_test = y[test_indices]

        if len(y_test) == 0:
            errors.append(0.0)
            continue

        model = get_model(method)
        model.fit(X_train, y_train)

        y_pred = model.predict(X_test)
        num_wrong = np.sum(y_pred != y_test)
        error_rate = num_wrong / len(y_test)
        errors.append(error_rate)

    return np.array(errors)
