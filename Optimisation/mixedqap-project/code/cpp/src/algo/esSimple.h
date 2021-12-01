#ifndef TEST_PLOT_PY_ESSIMPLE_H
#define TEST_PLOT_PY_ESSIMPLE_H


#include <mixedQAPeval.h>
#include <random>
#include "search.h"
#include <cmath>


class esSimple : public Search
{
protected:
    MixedQAPeval _eval;
    std::default_random_engine _rng;

public:
    esSimple(MixedQAPeval &eval, std::default_random_engine &rng) : _eval(eval), _rng(rng) {}

    virtual void operator()(Solution &solution)
    {
        std::normal_distribution<double> distribution(0, 1);

        double sigma=0.0005;
        double gamma=1.1;

        int iterations = 0;

        while (time(NULL) < _timeLimit && iterations < 200) {
            double cumul = 0;
            Solution sol = solution;
            for (int i = 0; i < solution.x.size(); i++) {
                double random = distribution(_rng);

                sol.x[i] += sigma * random;
                if (sol.x[i] < 0) {
                    sol.x[i] *= -1;
                }

                cumul += sol.x[i];
            }

            if (cumul == 0) {
                cumul = 0;
                for (int i = 0; i < sol.x.size(); i++) {
                    sol.x[i] = 1 / sol.x.size();
                    cumul += sol.x[i];
                }
            } else if (cumul != 1) {
                for (int i = 0; i < solution.x.size(); i++) {
                    solution.x[i] / cumul;
                }
            }

            sol.modifiedX = true;
            _eval.eval(solution);

            if (sol.fitness < solution.fitness){
                solution = sol;
                iterations = 0;
                sigma = sigma * gamma;
            } else {
                iterations++;
                sigma = sigma * pow(gamma,-1/4);
            }
        }
    }
};

#endif //TEST_PLOT_PY_ESSIMPLE_H
