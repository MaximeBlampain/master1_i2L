//
// Created by nfourny on 28/12/2020.
//

#ifndef PROJET_GLOUTON_2020_GAME_H
#define PROJET_GLOUTON_2020_GAME_H


#include <vector>
#include "Building.h"

class Game {
private:
    int _tailleTerrain;
    std::vector< std::vector<int> > _terrain;
    std::vector<Building> _buildings;

    int _fitness;
public:
    Game(std::string myfile);

    /* Initialiser */
    void init();
    /* Afficher */
    void print();

    /* Lire le fichier txt */
    void read(std::string myfile);
    /* Extraire uniquement les coordonnées (int) */
    static int extractIntegerFromString(std::string str, int nb);
    /* Savoir si la case est occupé */
    bool busy(int i,int j);

    void placementDefault();
    /* Savoir si l'on peut poser ou non un building */
    bool placementBusyForBuilding(Building building, int posX,int posY);
    /* Placement du building et de la route*/
    void placement(Building building, int posX,int posY);

    /* Algorithmes de tries */
    void variantesGloutons(int variante);
    int maxAire(std::vector<Building>);
    int minAire(std::vector<Building>);
    int maxEncombrement(std::vector<Building>);

    void eval();
};


#endif //PROJET_GLOUTON_2020_GAME_H
