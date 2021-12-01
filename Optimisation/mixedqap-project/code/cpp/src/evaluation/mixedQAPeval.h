#ifndef _mixedQAPeval_h
#define _mixedQAPeval_h

#include <iostream>
#include <fstream>
#include <vector>

#include "solution.h"

/**
    Evaluation function of the Mixed-variable QAP
*/
class MixedQAPeval {
public:
    MixedQAPeval(const char *fileName) {
        readInstance(fileName);
    }

    void operator()(Solution & solution) {
        solution.fitness = 0.0;

        if (solution.modifiedX) {
            for(unsigned i = 1; i < n; i++) {
                for(unsigned j = 0; j < i; j++) {
                    solution.flow[i][j] = flow0[i][j] + flow1[i][j] * solution.x[i] + flow2[i][j] * solution.x[j] + flow3[i][j] * solution.x[i] * solution.x[j];
                    if (solution.flow[i][j] > 0)
                        solution.fitness += solution.flow[i][j] * distance[ solution.sigma[i] ][ solution.sigma[j] ]; 
                    else
                        solution.flow[i][j] = 0;
                }
            }

            solution.modifiedX = false;
        } else {
            for(unsigned i = 1; i < n; i++) {
                for(unsigned j = 0; j < i; j++) {
                    solution.fitness += solution.flow[i][j] * distance[ solution.sigma[i] ][ solution.sigma[j] ];
                }
            }
        }
        solution.fitness *= 2;
    }

    void eval(Solution & solution) {
        operator()(solution);
    }

    void printOut(std::ostream & out) {
        out << n << std::endl;

        printMatrix(out, distance);
        out << std::endl;

        printMatrix(out, flow0);
        out << std::endl;
        printMatrix(out, flow1);
        out << std::endl;
        printMatrix(out, flow2);
        out << std::endl;
        printMatrix(out, flow3);
    }

    // dimension of the problem
    unsigned n;

    // distance matrix
    std::vector< std::vector<double> > distance;

    // flow matrices : flow = flow0 + flow1 * xi + flow2 * xj + flow3 xi * xj
    std::vector< std::vector<double> > flow0;
    std::vector< std::vector<double> > flow1;
    std::vector< std::vector<double> > flow2;
    std::vector< std::vector<double> > flow3;

protected:
    void readInstance(const char * fileName) {
        std::fstream f(fileName, std::ios::in);

        if (f) {
            f >> n ;

            readMatrix(f, distance);
            readMatrix(f, flow0);
            readMatrix(f, flow1);
            readMatrix(f, flow2);
            readMatrix(f, flow3);

            f.close();
        } else {
            std::cerr << "Error: Impossible to open " << fileName << std::endl;
        }

    }

    void readMatrix(std::fstream & f, std::vector< std::vector<double> > & m) {
        m.resize(n);

        for(unsigned i = 0; i < n; i++) {
            m[i].resize(n);
            for(unsigned j = 0; j < n; j++) {
                f >> m[i][j];
            }
        }
    }

    void printMatrix(std::ostream & out, std::vector< std::vector<double> > & m) {
        for(unsigned i = 0; i < n; i++) {
            out << m[i][0];
            for(unsigned j = 1; j < n; j++) {
                out << " " << m[i][j];
            }
            out << std::endl;
        }
    }
};

#endif