#ifndef GAMEIAMC_H
#define GAMEIAMC_H

#endif // GAMEIAMC_H

#include "game.h"
#include "MCTnode.h"

#include <iostream>
#include <fstream>
#include <math.h>
#include <float.h>
#include <utility>
#include <cstdlib>
#include "MCTnode.h"

#pragma once

class GameIAMc : public Game {

public:
    Game env_;
    MCTnode * root_ = new MCTnode();

    GameIAMc() : Game() {
        run();
    };

    GameIAMc(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run(){

        while(!win() && !end()){
            srand(time(NULL));
            std::vector<Directions> availablesMove = availableMove();
            Game gameCopy(game_,score_,bestTile_,playerTurn_);

            int action = 0;
            action = get_actions(gameCopy);
            if (!(std::count(availablesMove.begin(), availablesMove.end(), nextMove(action))))
                action = availablesMove[0];
            delay(10);
            move(nextMove(action));
        }
        if(!path_.empty())
            export_csv();

        std::cout << "Score : " << score() << std::endl;
        std::cout << "Meilleure tuile : " << bestTile() << std::endl;
    }

    int get_actions(Game & game) {
        root_  = new MCTnode(game.game_,game.score_);
        int depth = 200;
        int n = 0;
        for (int i = 0 ; i < depth ; i++) {

            MCTnode * new_node = select_expand();
            Game gameCopy(new_node->state_,new_node->score_);

            float value = evaluate(new_node,gameCopy);
            backup(new_node,value);

            n+=1;
        }
        int action = 0;
        float max_value = -10000000;
        for(int i = 0; i < 4; i++) {
            if(root_->child_.at(i) == nullptr){
                continue;
            }
            float value = root_->child_.at(i)->Q_;
            if(value > max_value){
                max_value = value;
                action = i;
            }
        }
        Game gameCopy2(game_,bestTile_);

        gameCopy2.move(gameCopy2.nextMove(action));

        root_ = root_->child_[action];
        delete root_->father_;

        return action;
      }

    MCTnode * select_expand() {
        Game gameCopy(game_,score_);
        MCTnode * current_node = root_;

        while (true){
            int action = 0;
            float max_value = -10000000;

            for(int i=0;i<4;i++){
                float value = _UCT(current_node,i);

                if(value > max_value){
                    max_value = value;
                    action = i;
                }
            }
            if (current_node->child_.at(action) != nullptr){
                current_node = current_node->child_.at(action);
                gameCopy.move(gameCopy.nextMove(action));
                continue;
            }
            else {
                gameCopy.move(gameCopy.nextMove(action));
                // Création noeux vide avec state gameCopy
                MCTnode * child_node = new MCTnode(gameCopy.game_,gameCopy.score_);
                // Copy current node into child node in father
                delete child_node->father_;
                child_node->father_ = current_node;
                current_node->child_[action] = child_node;
                return child_node;
            }
        }
    }

    float evaluate(MCTnode * node, Game & gameCopy) {
        int reward = gameCopy.score_;

        for (int i = 0 ; i < 20 ; i++) {
            int action = rand()%3;
            gameCopy.move(gameCopy.nextMove(action));
            if (gameCopy.win() && gameCopy.end())
                break;
            reward = cal_eval(gameCopy.game_,gameCopy.score_);
            reward = gameCopy.score_;
        }
        node->state_ = gameCopy.game_;
        return reward;
    }

    float _UCT(MCTnode * node,int action){
        if(node->child_[action] == nullptr){
            return 10000000;
        } else {
            return node->child_.at(action)->Q_ + 100*sqrt(log(node->N_)/node->child_.at(action)->N_);
        }
    }

    void backup(MCTnode * node,float value){
        while(node != nullptr){
            node->N_ += 1;
            node->Q_ = node->Q_ + (value-node->Q_)/node->N_;
            node = node->father_;
        }
    }

    int cal_eval(std::vector<std::vector<int>> game, int score)
    {
        return cal_weight(game) - cal_cluster(game) + score + cal_free(game) * cal_free(game);
    }

    int cal_weight(std::vector<std::vector<int>> game)
    {
        int w = 0;
        int weight[4][4] = {0, 0, 1, 3, 0, 1, 3, 5, 1, 3, 5, 15, 3, 5, 15, 30};
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j ++)
                w += game[i][j] * weight[i][j];
        return w / 16;
    }

    int cal_cluster(std::vector<std::vector<int>> game)
    {
        int p = 0;
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                for (int h = -1; h <= 1; h++)
                    for (int k = -1; k <=1; k++)
                    {
                        if (i + h > 3 || i + h < 0 || j + k > 3 || j + k < 0)
                            continue;
                        p += abs(game[i + h][j + k] - game[i][j]);
                    }
        return p / 2;
    }

    int cal_free(std::vector<std::vector<int>> game)
    {
        int f = 0;
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                if (!game[i][j])
                    f++;
        return f;
    }
};
