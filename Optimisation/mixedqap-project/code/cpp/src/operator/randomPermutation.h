#pragma once

#include "operator.h"

#include <random>
#include <algorithm>

class RandomPermutation : public Operator {
protected:
  std::default_random_engine _rng;
  unsigned int _n;

public:
  RandomPermutation(std::default_random_engine rng, unsigned int n) : _rng(rng), _n(n) {}

  void operator()(Solution &solution) {
    solution.sigma.resize(_n);

    for (unsigned i = 0; i < _n; i++)
      solution.sigma[i] = i;


    std::shuffle(solution.sigma.begin(), solution.sigma.end(), _rng);
  }
};