#include "game.h"
#include<iostream>
#include<fstream>

class GameIAHillClimbing : public Game {
private:

public:
    GameIAHillClimbing() : Game() {
        run();
    };

    GameIAHillClimbing(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run() {

       int best_score=0;
       Directions best_moves;
       std::vector<Directions> availableMoves;
       Game gameCopy(game_,score());

       while(!win() && !end()){
           gameCopy.game_ = game_;
           gameCopy.score_ = score_;
           availableMoves = availableMove();

           for (auto it = availableMoves.begin(); it != availableMoves.end(); ++it) {
               int score = 0;
               score = stochasticRecursive(gameCopy,*it);
               if(score>=best_score){
                   best_score = score;
                   best_moves = *it;
               }
               gameCopy.game_ = game_;
               gameCopy.score_ = score_;
           }

           move(best_moves);
           availableMoves.clear();
       }
       if(!path_.empty())
           export_csv();

       std::cout << "Score : " << score() << std::endl;
       std::cout << "Meilleure tuile : " << bestTile() << std::endl;
    }

    int stochasticRecursive(Game & game, Directions direction){
     game.move(direction);
     return game.score();
    }

};
