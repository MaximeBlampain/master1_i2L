#ifndef GAMEIARANDOM_H
#define GAMEIARANDOM_H

#endif // GAMEIARANDOM_H

#include "game.h"
#include <iostream>
#include <fstream>

class GameIARandom : public Game {


public:

    GameIARandom() : Game() {
        run();
    };

    GameIARandom(std::string path) : Game() {
        path_ = path;
        run();
    };

    void run() {
       while(!win() && !end()){
           Directions aleatoire = static_cast<Directions>(rand() % Right);
           move(aleatoire);
       }
       if(!path_.empty())
           export_csv();

      std::cout << "Score : " << score() << std::endl;
      std::cout << "Meilleure tuile : " << bestTile() << std::endl;
    }
};
