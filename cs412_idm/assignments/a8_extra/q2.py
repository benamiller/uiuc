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
    
    return model

def my_train_test(method, X, y, pi, k):
    errors = []
    # Implement your code to construct the list of errors here
    # Do NOT change the return statement

    return np.array(errors)