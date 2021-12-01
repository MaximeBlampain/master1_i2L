#ifndef GAMEIAMONTECARLO_H
#define GAMEIAMONTECARLO_H

#endif // GAMEIAMONTECARLO_H

#include "game.h"
#include <iostream>
#include <fstream>

class GameIAMontecarlo : public Game {


public:
    GameIAMontecarlo() : Game() {
        run();
    };

    GameIAMontecarlo(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run(){
        while(!win() && !end()){
            Directions bestMove = iaMontecarlo();
            delay(10);
            move(bestMove);
        }
        if(!path_.empty())
            export_csv();

        std::cout << "Score : " << score() << std::endl;
        std::cout << "Meilleure tuile : " << bestTile() << std::endl; 
    }

    Directions iaMontecarlo(){
        Game gameCopy(game_,score_);

        int best_index = -1;
        int best_score_moyenne = -1;
        std::vector<Directions> availableMoves = gameCopy.availableMove();

        for (auto it = availableMoves.begin(); it != availableMoves.end(); ++it) {
            Directions direction = nextMove(*it);
            int bestScoreMoyen = bestScoreMoyenne(gameCopy,direction);

            if(bestScoreMoyen >= best_score_moyenne){
                best_index = *it;
                best_score_moyenne = bestScoreMoyen;
            }
        }
        if(best_index == -1)    return nextMove(0);

        return nextMove(best_index);
    }

    int bestScoreMoyenne(Game & gameCopy, Directions direction){
        Game gameCopy2(gameCopy.game_,score_);
        gameCopy2.move(direction);

        int total_score = 0;
        if(!gameCopy2.win() && !gameCopy2.end()){
            for(int i = 0 ; i<100; i++){
                int game_score = gameCopy2.score_;
                Game gameCopy3(gameCopy2.game_,game_score);

                while(!gameCopy3.win() && !gameCopy3.end()){
                    std::vector<Directions> availableMoves = gameCopy3.availableMove();
                    Directions aleatoire = availableMoves[rand() % availableMoves.size()];
                    gameCopy3.move(aleatoire);

                    game_score = gameCopy3.score_;
                }
                total_score += game_score ;
            }
            return total_score / 100;
        }
        return total_score;
    }
};
