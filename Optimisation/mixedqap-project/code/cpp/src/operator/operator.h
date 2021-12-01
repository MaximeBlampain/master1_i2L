#pragma once

#include "../evaluation/solution.h"

class Operator {
    public:
        virtual void operator()(Solution &solution) = 0;
};