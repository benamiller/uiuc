// Submit this file to Gradescope
#include "submission.h"

using std::vector;
using std::unordered_map;
using std::string;

int num_C = 7; // Represents the total number of classes

vector<double> Solution::prior(const vector<vector<int>> &X_train, const vector<int> &Y_train)
{
    // Calculate the prior probabilities of each class
    // Args:
    //   X_train: Row i represents the i-th training datapoint
    //   Y_train: The i-th integer represents the class label for the i-th training datapoint
    // Returns:
    //   A vector of length C where C is the number of classes in the dataset
    
    // Implement this function
}

vector<int> Solution::label(const vector<vector<int>> &X_train, const vector<int> &Y_train, const vector<vector<int>> &X_test)
{
    // Calculate the classification labels for each test datapoint
    // Args:
    //   X_train: Row i represents the i-th training datapoint
    //   Y_train: The i-th integer represents the class label for the i-th training datapoint
    //   X_test: Row i represents the i-th testing datapoint
    // Returns:
    //   A vector of length M where M is the number of datapoints in the test set
    
    // Implement this function
}