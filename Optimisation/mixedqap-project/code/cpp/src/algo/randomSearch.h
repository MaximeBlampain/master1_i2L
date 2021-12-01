#pragma once

#include "../operator/randomPermutation.h"
#include "../operator/uniformContinue.h"
#include "search.h"

#include <random>

class RandomSearch : public Search {
protected:
  std::default_random_engine &_rng;
  MixedQAPeval _eval;

public:
  RandomSearch(std::default_random_engine &rng, MixedQAPeval &eval): _rng(rng), _eval(eval) {}

  virtual void operator()(Solution &solution) {
    RandomPermutation random(_rng, solution.sigma.size());
    UniformContinue uniform(solution.x.size());

    Solution tmpSolution(solution.sigma.size());
    uniform(tmpSolution);

    while (time(NULL) < _timeLimit) {
      random(tmpSolution);

      _eval.eval(tmpSolution);

      if (tmpSolution.fitness < solution.fitness) {
        solution = tmpSolution;
      }
    }
  }
};