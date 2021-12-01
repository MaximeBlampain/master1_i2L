/*

 Author: 
  Sebastien Verel, 
  Univ. du Littoral Côte d'Opale, France.
  version 0 : 2020/11/24

    Mixed QAP
    

To compile:

mkdir build
cd build
cmake ../src/tests
make

To execute:

./t-solution

*/

#include <iostream>
#include <fstream>
#include <stdlib.h>

#include <solution.h>

using namespace std;

/**
 *  Main function
 */
int main(int argc, char **argv) {
    //---------------------------------
    // *** Arguments

    unsigned n = 4;

    Solution solution(n);

    for(unsigned i = 0; i < n; i++) {
        solution.x[i] = 1.0 / n;
        solution.sigma[i] = i;
    }

    cout << solution << endl;

    //----------------------------------
    // ok

    return 1;
}
