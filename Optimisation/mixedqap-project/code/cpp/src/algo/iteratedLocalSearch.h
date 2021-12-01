#pragma once

#ifndef MIXEDQAP_ITERATEDLOCALSEARCH_H
#define MIXEDQAP_ITERATEDLOCALSEARCH_H

#include <random>
#include "hillClimbing.h"
#include "search.h"

class IteratedLocalSearch : public Search {
protected:
    MixedQAPeval _eval;
    std::default_random_engine & _rng;

public:
    IteratedLocalSearch(MixedQAPeval &eval, std::default_random_engine &rng) : _eval(eval), _rng(rng) {}

    virtual void operator()(Solution &solution) {
        HillClimbing HillClimbing(_eval);
        HillClimbing.timeLimit(time(NULL) + 1);

        std::uniform_int_distribution<int> distribution(0,solution.sigma.size() - 1);
        HillClimbing(solution);

        while (time(NULL) < _timeLimit) {
            Solution solPrime = solution;

            unsigned k1 = distribution(_rng);
            unsigned k2 = distribution(_rng);
            std::swap(solPrime.sigma[k1], solPrime.sigma[k2]);

            HillClimbing.timeLimit(time(nullptr) + 1);
            HillClimbing(solPrime);

            if (solPrime.fitness < solution.fitness) {
                solution = solPrime;
                _eval.eval(solution);
            }
        }
    }
};

#endif //MIXEDQAP_ITERATEDLOCALSEARCH_H
