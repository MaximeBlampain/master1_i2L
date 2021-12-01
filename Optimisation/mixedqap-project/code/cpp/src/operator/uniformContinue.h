#pragma once

#include "operator.h"

class UniformContinue : public Operator {
protected:
  unsigned int _n;

public:
  UniformContinue(unsigned int n) : _n(n) {}

  void operator()(Solution &solution) {
    solution.x.resize(_n);

    for (unsigned i = 0; i < _n; i++)
      solution.x[i] = 1.0 / _n;

    solution.modifiedX = true;
  }
};