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

./t-eval

*/

#include <iostream>
#include <fstream>
#include <stdlib.h>

#include <solution.h>
#include <mixedQAPeval.h>

using namespace std;

/**
 *  Main function
 */
int main(int argc, char **argv) {
    //---------------------------------
    // *** Arguments

    char instanceFileName[256] = "../../../instances/mixedQAP_uni_6_-100_100_1.dat";

    MixedQAPeval eval(instanceFileName);

    eval.printOut(std::cout);
    std::cout << std::endl;

    Solution solution(eval.n);

    for(unsigned i = 0; i < eval.n; i++) {
        solution.x[i] = 1.0 / eval.n;
        solution.sigma[i] = i;
    }

    eval(solution);

    cout << solution << endl;

    cout << endl;
    solution.printOnFlow(std::cout);

    //----------------------------------
    // ok

    return 1;
}
