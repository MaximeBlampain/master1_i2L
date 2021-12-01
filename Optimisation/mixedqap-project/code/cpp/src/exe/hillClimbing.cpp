#include "../algo/hillClimbing.h"
#include "../operator/randomPermutation.h"
#include "../operator/uniformContinue.h"

#include <iostream>

int main(int argc, char *argv[]) {
    auto randomGenerator = std::default_random_engine{};
    randomGenerator.seed(atoi(argv[2]));

    MixedQAPeval eval(argv[1]);
    RandomPermutation init(randomGenerator, eval.n);
    UniformContinue uniform(eval.n);
    HillClimbing hillClimber(eval);
    Solution bestSolution(eval.n);

    init(bestSolution);
    uniform(bestSolution);
    eval.eval(bestSolution);
    hillClimber.timeLimit(time(NULL) + atoi(argv[3]));

    hillClimber(bestSolution);

    std::cout.precision(15);
    std::cout << bestSolution.fitness << std::endl;

    return 0;
}
