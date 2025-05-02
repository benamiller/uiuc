// This file will be available at the autograder runtime
// Do NOT upload this file in your submission

#include <vector>
#include <unordered_map>
using namespace std;

class Solution
{
 public:
  vector<double> Solution::prior(const vector<vector<int>> &X_train, const vector<int> &Y_train);
  vector<int> Solution::label(const vector<vector<int>> &X_train, const vector<int> &Y_train, const vector<vector<int>> &X_test);
};