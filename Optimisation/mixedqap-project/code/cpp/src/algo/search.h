#pragma once

#include "../evaluation/mixedQAPeval.h"

#include <ctime>

class Search {
protected:
  time_t _timeLimit;

public:
  Search() {}

  virtual void operator()(Solution &_solution) = 0;

  virtual void timeLimit(time_t limit) { _timeLimit = limit; }
};