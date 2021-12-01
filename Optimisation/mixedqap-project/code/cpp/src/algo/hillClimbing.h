#pragma once

#include "../evaluation/neighboorEval.h"
#include "search.h"

class HillClimbing : public Search {
protected:
    std::vector<std::vector<double>> _delta;
    MixedQAPeval _eval;

public:
    HillClimbing(MixedQAPeval &eval) : _eval(eval) {}

    virtual void operator()(Solution &solution) {
        bool optimumLocal = false;

        while (time(NULL) < _timeLimit && !optimumLocal) {
            Solution bestSolution = solution;

            double bestFitness = bestSolution.fitness;

            neighboorEval eval;
            eval.init(solution, _delta);
            for (unsigned int i = 0; i < solution.sigma.size(); i++) {
                for (unsigned int j = (i + 1); j < solution.sigma.size(); j++) {

                    std::swap(solution.sigma[i], solution.sigma[j]);

                    eval.update(solution, std::make_pair(i, j), _delta);
                    _eval.eval(solution);

                    if (solution.fitness < bestFitness) {
                        bestFitness = solution.fitness;
                        bestSolution = solution;
                    }

                    std::swap(solution.sigma[i], solution.sigma[j]);
                    _eval.eval(solution);

                }
            }

            if (bestSolution.fitness < solution.fitness) {
                solution = bestSolution;
                _eval.eval(solution);
            } else {
                optimumLocal = true;
            }
        }
    }
};
