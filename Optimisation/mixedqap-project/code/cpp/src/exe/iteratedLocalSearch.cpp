#include "../algo/iteratedLocalSearch.h"
#include "../operator/randomPermutation.h"
#include "../operator/uniformContinue.h"

#include <iostream>

int main(int argc, char *argv[]) {
    auto randomGenerator = std::default_random_engine{};
    randomGenerator.seed(atoi(argv[2]));

    MixedQAPeval eval(argv[1]);
    RandomPermutation init(randomGenerator, eval.n);
    UniformContinue uniform(eval.n);
    IteratedLocalSearch iteratedLocalSearch(eval, randomGenerator);
    Solution bestSolution(eval.n);

    init(bestSolution);
    uniform(bestSolution);
    eval.eval(bestSolution);
    iteratedLocalSearch.timeLimit(time(NULL) + atoi(argv[3]));

    iteratedLocalSearch(bestSolution);

    std::cout.precision(15);
    std::cout << bestSolution.fitness << std::endl;

    return 0;
}
