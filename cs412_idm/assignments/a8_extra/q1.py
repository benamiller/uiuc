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
    
    return model

def get_splits(n, k, seed):
    splits = None
    # Implement your code to construct the splits here
    # Do NOT change the return statement

    return splits

def my_cross_val(method, X, y, splits):
    errors = []
    # Implement your code to construct the list of errors here
    # Do NOT change the return statement
    
    return np.array(errors)