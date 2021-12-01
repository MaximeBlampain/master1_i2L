#include <iostream>

#include <hillClimbingBestImprovement.h>

using namespace std;

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <nbRuns> <instanceFileName.dat>" << std::endl;
        return -1;
    }

    unsigned nbRuns = atoi(argv[1]);
    char* instanceFileName = argv[2];

    H optimizer(instanceFileName);

    double maxFitness = 0;
    for(unsigned i=0 ; i<nbRuns ; i++) {
        Solution bestSolution = optimizer.run();
        std::cout << "Fitness at run " << (i+1) << ": " << bestSolution.fitness << std::endl;
        if(bestSolution.fitness > maxFitness) {
            maxFitness = bestSolution.fitness;
        }
    }

    std::cout << "\nBest fitness found: " << maxFitness << std::endl;

    return 0;
}
