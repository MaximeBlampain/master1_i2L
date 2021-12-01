#ifndef _solution_h
#define _solution_h

#include <iostream>
#include <vector>

/**
   Classe pour représenter une solution à variable mixte:
     vector of double and vector of permutation
     vector de la permutation

*/
class Solution {
public:
  Solution() {
    fitness = 0.0;
  }

  Solution(unsigned _n) {
    x.resize(_n);

    sigma.resize(_n);

    fitness = 0.0;

    modifiedX = true;

    flow.resize(_n);

    for(unsigned i = 0; i < _n; i++)
      flow[i].resize(_n);
  }

  Solution(const Solution & _s) {
    fitness = _s.fitness;

    x.resize(_s.x.size());

    for(unsigned i = 0; i < x.size(); i++)
      x[i] = _s.x[i];

    sigma.resize(_s.sigma.size());

    for(unsigned i = 0; i < sigma.size(); i++)
      sigma[i] = _s.sigma[i];

    modifiedX = _s.modifiedX;

    flow.resize(sigma.size());
    if (!_s.modifiedX) {
      // copy the personnal flow matrix
      for(unsigned i = 0; i < sigma.size(); i++) {
        flow[i].resize(sigma.size());
        for(unsigned j = 0; j < i; j++)
          flow[i][j] = _s.flow[i][j];
      }
    } else {
      for(unsigned i = 0; i < sigma.size(); i++) {
        flow[i].resize(sigma.size());
      }
    }

  }

  Solution& operator=(const Solution & _s) {
    fitness = _s.fitness;

    x.resize(_s.x.size());

    for(unsigned i = 0; i < x.size(); i++)
      x[i] = _s.x[i];

    sigma.resize(_s.sigma.size());

    for(unsigned i = 0; i < sigma.size(); i++)
      sigma[i] = _s.sigma[i];

    modifiedX = _s.modifiedX;

    flow.resize(sigma.size());
    if (!_s.modifiedX) {
      // copy the personnal flow matrix
      for(unsigned i = 0; i < sigma.size(); i++) {
        flow[i].resize(sigma.size());
        for(unsigned j = 0; j < i; j++)
          flow[i][j] = _s.flow[i][j];
      }
    } else {
      for(unsigned i = 0; i < sigma.size(); i++) {
        flow[i].resize(sigma.size());
      }
    }

    return *this;
  }

  std::vector<Solution> neighbors() {
    std::vector<Solution> neighbors;

    for(unsigned i=0 ; i<sigma.size() ; i++) {
      for (unsigned j=i; j<sigma.size() ; j++) {
        Solution tmpSolution(*this);
        std::swap(tmpSolution.sigma[i], tmpSolution.sigma[j]);
        neighbors.push_back(tmpSolution);
      }
    }

    return neighbors;
  }

  /**
   * print the solution
   */
  virtual void printOn(std::ostream& _os) const {
    _os << fitness ;

    _os << " " << x.size() ;

    for(auto xi : x)
      _os << " " << xi ;

    for(auto si : sigma)
      _os << " " << si ;
  }

  /**
   * print the flow matrix
   */
  virtual void printOnFlow(std::ostream& _os) const {
      _os << modifiedX << std::endl;

      for(unsigned i = 0; i < sigma.size(); i++) {
        _os << flow[i][0] ;
        for(unsigned j = 1; j < sigma.size(); j++)
          _os << " " << flow[i][j] ;
        _os << std::endl;
      }
  }

  // vector of real numbers
  std::vector<double> x;

  // permutation
  std::vector<unsigned> sigma;

  // fitness value
  double fitness;

  // true when the vector x have been modified
  bool modifiedX;

  // personnal flow matrix when x is constant
  std::vector< std::vector<double> > flow;

};

std::ostream & operator<<(std::ostream& _os, const Solution & _s) {
    _s.printOn(_os);
    return _os;
}

#endif
