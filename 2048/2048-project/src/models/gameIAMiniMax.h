#ifndef GAMEIAMINIMAX_H
#define GAMEIAMINIMAX_H

#endif // GAMEIAMINIMAX_H

#include "game.h"
#include <iostream>
#include <fstream>
#include <math.h>

class GameIAMiniMax : public Game {

public:
    GameIAMiniMax() : Game() {
        run();
    };

    GameIAMiniMax(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run(){
        int depth = 7;
        while(!win() && !end()){
            srand(time(NULL));
            Game gameCopy(game_,score_,bestTile_,playerTurn_);

            std::vector<Directions> availablesMove = gameCopy.availableMove();
            int max_move = getMoveIndex(availablesMove[0]);
            int max_score = -10000000;

            for(int i = 0 ; i < depth ; i++){
                std::vector<int> result = search(gameCopy, -10000000, 10000000, 1, i);
                if(result[1] > max_score) {
                    max_score = result[1];
                    max_move = result[0];
                }
            }
            delay(10);
            if (!(std::count(availablesMove.begin(), availablesMove.end(), nextMove(max_move))))
                max_move = availablesMove[0];

            move(nextMove(max_move));
        }
        if(!path_.empty())
            export_csv();

        std::cout << "Score : " << score() << std::endl;
        std::cout << "Meilleure tuile : " << bestTile() << std::endl;
    }

    int evaluate(Game & game){
        int empty = game.availableCells().size();
        int position = max_tile_position(game);
        int weighted_sum = weighted_board(game);
        int smooth = smoothness(game);
        int mono = monotonicity(game);

        return empty + position + weighted_sum + smooth + mono;
    }

    int max_tile_position(Game & game) {
        if (game.game_[0][0] == 2048) {
            return MAX_TILE_CREDIT;
        } else {
            return -MAX_TILE_CREDIT;
        }
    }

    int weighted_board(Game & game){
            int result = 0;
            for (int i = 0; i < 4; i++)
                for (int j = 0; j < 4; j ++)
                    result += game.game_[i][j] * WEIGHT_MATRIX[i][j];
            return result;
    }

    int smoothness(Game & game) {
        int smoothness = 0;
        std::vector<int> rows = row_sum(game.game_);

        for (int i = 0; i < 3; i++)
            smoothness += abs(rows[i] - rows[i+1]);

        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 3; j++)
                smoothness += abs(game.game_[j][i] - game.game_[j + 1][i]);

        return smoothness;
    }

    std::vector<int> row_sum(std::vector<std::vector<int>> game){
        int i,j,sum = 0;
        std::vector<int> rows;

        for (i = 0; i < gridSize; ++i) {
            for (j = 0; j < gridSize; ++j) {
                sum = sum + game[i][j];
            }
            rows.push_back(sum);
            sum = 0;
        }
        return rows;
    }

    int monotonicity(Game & game){
            int mono = 0;
            std::vector<int> rows = row_sum(game.game_);

            for (int i = 0 ; i < gridSize ; i++) {
                int diff = rows[0] - rows[1];
                for (int j = 0 ; j < 3 ; j++) {
                    if ((rows[j] - rows[j + 1] * diff) <= 0){
                        mono += 1;
                    } else {
                        diff = rows[j] - rows[j + 1];
                    }
                }
            }

            for (int i = 0 ; i < gridSize ; i++) {
                int diff = game.game_[0][i] - game.game_[1][i];
                   for (int j = 0 ; j < 3 ; j++) {
                   if ((game.game_[j][i] - game.game_[j + 1][i]) * diff <= 0) {
                    mono += 1;
                   } else {
                    diff = game.game_[j][i] - game.game_[j + 1][i];
                   }
                }
            }
            return mono;
    }

    std::vector<int> search(Game & game,int alpha,int beta,int depth,int max_depth) {
            if (depth > max_depth or game.end()) {
                std::vector<int> tmpResult = {1,evaluate(game)};
                return tmpResult;
            }

            if (game.playerTurn_) {
            std::vector<Directions> availablesMove = game.availableMove();
            int result_move = getMoveIndex(availablesMove.at(0));
                int v = -10000000;
                for (auto it = availablesMove.begin(); it != availablesMove.end(); ++it) {
                    Game gameCopy(game.game_,game.score_,game.bestTile_,game.playerTurn_);
                    gameCopy.moveWithoutAddRandomTiles(*it);
                    gameCopy.playerTurn_ = false;
                    int prev_v = v;

                    std::vector<int> result = search(gameCopy,alpha,beta,depth+1,max_depth);
                    v = std::max(v, result[1]);

                    if (v > prev_v and depth == 1){
                        result_move = getMoveIndex(*it);
                    }
                    if (v >= beta) {
                        return {result_move,v};
                    }
                    alpha = std::max(alpha,v);
                }
                if (depth == 1){
                    std::vector<int> resultTmp = {result_move,v};
                    return resultTmp;
                }
                std::vector<int> resultTmp = {result_move,v};
                return resultTmp;
            } else {
                std::vector<int> availableCells = game.availableCells();
                int v = 10000000;

                for(int i = 0; i < availableCells.size() ;i++) {
                    Game gameCopy(game.game_,game.score_,game.bestTile_,game.playerTurn_);

                    int x = (floor(availableCells.at(i) / gridSize));
                    int y = (availableCells.at(i) % gridSize);

                    std::srand(static_cast<unsigned int>(std::time(nullptr)));
                    float myRandom = std::rand() / RAND_MAX;
                    if(myRandom < 0.90){
                       gameCopy.game_[x][y] = 2;
                    } else{
                        gameCopy.game_[x][y] = 4;
                    }
                    gameCopy.playerTurn_ = true;

                    std::vector<int> result = search(gameCopy,alpha,beta,depth+1,max_depth);
                    v = std::min(v, result[1]);
                    if (v <= alpha){
                        return {v,-1};
                    }
                    beta = std::min(beta, v);
                }
                if(depth == 1) {
                    std::vector<int> resultTmp = {-1,v};
                    return resultTmp;
                }
                std::vector<int> resultTmp = {-1,v};
                return resultTmp;
            }
    }

    int getMoveIndex(Directions direction){
        if(direction==Up)
            return 0;

        if(direction==Left)
            return 1;

        if(direction==Down)
            return 2;

        if(direction==Right)
            return 3;
    }

};
