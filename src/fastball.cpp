#include <Rcpp.h>
#include <time.h>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List fastball_cpp(Rcpp::List inputDf, Rcpp::NumericVector dim) {
  //convert input list into std::vector of std::vectors

  int numRows = dim[0];
  std::vector<std::vector<int>> oneLocs (numRows);
  for(int i = 0; i < numRows; i++) {
    //Method 1:
    oneLocs[i] = Rcpp::as<std::vector<int>> (inputDf[i]);

    /*
     //Method 2:
     Rcpp::NumericVector temp = inputDf[i];
     for (int j = 0; j < temp.size(); j++) {
     oneLocs[i].push_back(temp[j]);

     }
     */
  }

  for (int i = 1; i <= 5 * numRows; i++) {

    //get two random row numbers
    int r1Index = rand() % numRows;
    int r2Index = r1Index;
    while (r2Index == r1Index) {
      r2Index = rand() % numRows;
    }

    //create references to the two rows being mixed
    std::vector<int> & r1 = oneLocs[r1Index];
    std::vector<int> & r2 = oneLocs[r2Index];
    if (r1.size() == 0 || r2.size() == 0) {
      continue;
    }

    //generate iterators for first pass through rows
    auto first1 = r1.begin();
    auto last1 = r1.end();
    auto first2 = r2.begin();
    auto last2 = r2.end();
    int intersectionLength = 0;


    //find the length of the intersection
    while (first1!=last1 && first2!=last2)
    {
      if (*first1<*first2) ++first1;
      else if (*first2<*first1) ++first2;
      else {
        intersectionLength += 1;
        ++first1; ++first2;
      }
    }

    //calculate length of symmetric difference
    int r1SymDiffSize = r1.size() - intersectionLength;
    int r2SymDiffSize = r2.size() - intersectionLength;
    int symDiffSize = r1SymDiffSize + r2SymDiffSize;

    if (symDiffSize == 0) {
      continue;
    }

    //create vector of zeros and ones
    std::vector<int> swapLocations (symDiffSize);
    std::fill(swapLocations.begin(), swapLocations.begin() + r1SymDiffSize, 0);
    std::fill(swapLocations.begin() + r1SymDiffSize, swapLocations.end(), 1);


    //shuffle swapLocations
    for (int i = 0; i < swapLocations.size() - 1; i++) {
      int j = i + rand() % (swapLocations.size() - i);
      std::swap(swapLocations[i],swapLocations[j]);
    }

    //create vectors to store output of curveball swaps
    std::vector<std::vector<int>> curveballRows (2);
    curveballRows[0].reserve(r1.size());
    curveballRows[1].reserve(r2.size());

    //generate iterators for sweep through r1 and r2
    first1 = r1.begin();
    last1 = r1.end();
    first2 = r2.begin();
    last2 = r2.end();
    auto swapIterator = swapLocations.begin();




    while (first1!=last1 && first2!=last2)
    {
      if (*first1<*first2) {
        curveballRows[*swapIterator].push_back(*first1);
        ++swapIterator;
        ++first1;
      }
      else if (*first2<*first1) {
        curveballRows[*swapIterator].push_back(*first2);
        ++swapIterator;
        ++first2;
      }
      else {
        curveballRows[0].push_back(*first1);
        curveballRows[1].push_back(*first2);
        ++first1; ++first2;
      }
    }

    while (first1 != last1) {
      curveballRows[*swapIterator].push_back(*first1);
      ++swapIterator;
      ++first1;
    }

    while (first2 != last2) {
      curveballRows[*swapIterator].push_back(*first2);
      ++swapIterator;
      ++first2;
    }
    /*
     for (int i = 0; i < curveballRows[0].size(); i++) {
     Rcpp::Rcout << curveballRows[0][i] << ' ';
     }
     Rcpp::Rcout << "\n";

     for (int i = 0; i < curveballRows[1].size(); i++) {
     Rcpp::Rcout << curveballRows[1][i] << ' ';
     }
     Rcpp::Rcout << "\n";
     */

    r1.clear();
    r2.clear();
    std::vector<int> & newV1 = curveballRows[0];
    std::vector<int> & newV2 = curveballRows[1];
    //Rcpp::Rcout << r1.size() << ' ' << curveballRows[0].size() << '\n';
    //Rcpp::Rcout << r2.size() << ' ' << curveballRows[1].size() << '\n';

    r1.insert(r1.end(), newV1.begin(), newV1.end());
    r2.insert(r2.end(), newV2.begin(), newV2.end());
  }
  for (int i = 0; i < numRows; i++) {
    Rcpp::NumericVector temp = Rcpp::wrap(oneLocs[i]);
    inputDf[i] = temp;
  }
  return inputDf;
}


