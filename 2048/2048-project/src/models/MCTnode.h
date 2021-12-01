#ifndef MCTNODE_H
#define MCTNODE_H

#endif // MCTNODE_H

#include <QObject>
#include <QStringList>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <random>

#pragma once

class MCTnode {

public:
    std::vector<std::vector<int>> state_;
    int score_;
    float Q_;
    int N_;
    std::vector<MCTnode*> child_;
    MCTnode * father_;

    MCTnode(){}

    MCTnode(std::vector<std::vector<int>> s, int score) {
        state_ = s;
        Q_ = 0.0;
        N_ = 0;
        for(int i = 0 ; i < 4 ; i++){
            child_.push_back(nullptr);
        }
        father_ = nullptr;
        score_ = score;
    }
};
