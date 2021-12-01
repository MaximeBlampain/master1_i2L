#ifndef MIXEDQAP_NEIGHBOOREVAL_H
#define MIXEDQAP_NEIGHBOOREVAL_H

#include "solution.h"
#pragma once

class neighboorEval {
public:
    void init(Solution &solution, std::vector<std::vector<double>> &delta) {
        delta.resize(solution.sigma.size());
        for (unsigned i = 0; i < solution.sigma.size(); i++) {
            delta[i].resize(solution.sigma.size());

            for (unsigned j = 0; j < solution.sigma.size(); j++) {
                delta[i][j] = solution.fitness;
            }

        }
    }

    void update(Solution &solution,std::pair<unsigned int, unsigned int> neighbor,std::vector<std::vector<double>> &delta) {
        delta[neighbor.first][neighbor.second] = solution.fitness - delta[neighbor.first][neighbor.second];
    }
};


#endif //MIXEDQAP_NEIGHBOOREVAL_H
