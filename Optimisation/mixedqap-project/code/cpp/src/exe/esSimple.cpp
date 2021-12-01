#include <iostream>
#include <stdlib.h>
#include <random>

#include <solution.h>
#include <mixedQAPeval.h>
#include <randomPermutation.h>
#include <esSimple.h>
#include <uniformContinue.h>

using namespace std;

int main(int argc, char **argv) {
    char * fileName = argv[1];
    unsigned int seed = atoi(argv[2]);
    unsigned int duration = atoi(argv[3]);

    auto rng = std::default_random_engine {};
    rng.seed(seed);

    MixedQAPeval eval(fileName);
    RandomPermutation init(rng, eval.n);
    UniformContinue uniform(eval.n);
    esSimple search(eval, rng);

    Solution solution(eval.n);
    init(solution);
    uniform(solution);
    eval(solution);
    search.timeLimit(time(NULL) + duration);
    search(solution);

    std::cout.precision(15);

    std::cout << solution.fitness << std::endl;
    return 1;
}
