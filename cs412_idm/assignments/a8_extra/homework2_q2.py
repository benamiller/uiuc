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
    # This is the same as Q1
    # Do NOT change the return statement
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

def my_train_test(method, X, y, pi, k):
    errors = []
    # Implement your code to construct the list of errors here
    # Do NOT change the return statement
    n = len(y)

    X = np.asarray(X)
    y = np.asarray(y)

    n_train = int(np.floor(pi * n))
    if n_train <= 0 or n_train >= n:
        raise ValueError("Invalid")

    all_indices = np.arange(n)

    for _ in range(k):
        shuffled_indices = np.random.permutation(all_indices)

        train_indices = shuffled_indices[:n_train]
        test_indices = shuffled_indices[n_train:]

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
