#ifndef GAMEIAGENETICALGORITHM_H
#define GAMEIAGENETICALGORITHM_H

#endif // GAMEIAGENETICALGORITHM_H

#include "game.h"
#include <iostream>
#include <fstream>

class GameIAExpectImax : public Game {

public:
    GameIAExpectImax() : Game() {
        run();
    };

    GameIAExpectImax(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run(){
        while(!win() && !end()){
            srand(time(NULL));
            Game gameCopy(game_,score_);

            int depth;
            if (cal_free(gameCopy.game_) <= 4){
                depth = 6;
            } else {
                depth = 5;
            }
            int action = best_move(gameCopy,depth);
            if(action == 0){
                action = rand() % 3;
            }
            delay(10);
            move(nextMove(action));
        }
        if(!path_.empty())
            export_csv();

        std::cout << "Score : " << score() << std::endl;
        std::cout << "Meilleure tuile : " << bestTile() << std::endl;
    }

    int expectimax(Game & game, int depth, bool flag) {
        Game copyGame(game.game_,game.score_);

        if (depth == 0)
        {
            if (copyGame.win() || copyGame.end())
                return -1;
            else {
                int eval = cal_eval(copyGame.game_,copyGame.score_);
                return eval;
            }
        }

        if (flag)
        {
            int best = -1;
            for (int i = 0; i < 4; i++) {
                Game copyGame2(copyGame.game_,copyGame.score_);
                std::vector<std::vector<int>> gameTmp = copyGame2.game_;
                copyGame2.move(copyGame2.nextMove(i));
                if(!test_move(copyGame2.game_,gameTmp))
                    continue;

                int eval = expectimax(copyGame2, depth - 1, 0);
                if (eval > best)
                    best = eval;
            }
            return best;
        } else {
            int sum_eval = 0, count = 0;
            for (int p = 0; p < 4; p++)
                for (int q = 0; q < 4; q++) {

                    Game copyGame3(copyGame.game_,copyGame.score_);

                    if (copyGame3.game_[p][q] != 0)
                        continue;

                    copyGame3.game_[p][q] = 2;
                    sum_eval += expectimax(copyGame3, depth - 1, 1);
                    count ++;
                }
            if (!count)
                return expectimax(copyGame, depth - 1, 1);

            return sum_eval / count;
        }
    }

    int best_move(Game & game, int depth)
    {
        Game copyGame(game.game_,game.score_);

        int best = -1, action = 0;

        for (int i = 0; i < 4; i++)
        {
            Game copyGame2(copyGame.game_,copyGame.score_);

            std::vector<std::vector<int>> gameTmp = copyGame2.game_;
            copyGame2.move(copyGame2.nextMove(i));
            if(!test_move(gameTmp,copyGame2.game_))
                continue;

            int eval = expectimax(copyGame2, depth - 1, 0);
            if (eval > best)
            {
                best = eval;
                action = i;
            }
        }

        return action;
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

    bool test_move(std::vector<std::vector<int>> game, std::vector<std::vector<int>> gameTest)
    {
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                if (game[i][j] != gameTest[i][j])
                    return 1;

        return 0;
    }

};
