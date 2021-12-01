//
// Created by nfourny on 28/12/2020.
//

#include <iostream>
#include <fstream>
#include <string.h>     /* atoi */
#include <sstream>
#include "Game.h"
#include "Building.h"

Game::Game(std::string myfile) {
    _fitness = 0;
    read(myfile);
    init();
}

void Game::init() {
    _terrain.resize(_tailleTerrain);
    for(int i = 0; i<_tailleTerrain; i++){
        _terrain[i].resize(_tailleTerrain);
        for(int j = 0; j<_tailleTerrain; j++){
            _terrain[i][j] = 0;
        }
    }
}

void Game::print() {
    if(_fitness != 0) {
        std::cout << "Evaluation : " << _fitness << std::endl;
    }
    for(int i = 0; i<_tailleTerrain; i++){
        for(int j = 0; j<_tailleTerrain; j++){
            if(_terrain[i][j] == -1){
                std::cout << "  | " << _terrain[i][j];
            } else {
                std::cout << "  |  " << _terrain[i][j];
            }
        }
        std::cout << "  |  " << std::endl;
    }
}

bool Game::busy(int i,int j) {
    if(_terrain[i][j] == 0){
        return false;
    } else {
        return true;
    }
}

int Game::extractIntegerFromString(std::string str, int nb) {
    std::stringstream ss(str);
    std::string tempStr;
    int found;
    int i = 0;

    while (ss >> tempStr) {
        if (std::stringstream(tempStr) >> found) {
            if (i == nb) {
                return found;
            }
        }
        i++;
    }
}

void Game::read(std::string myfile) {
    std::string line;
    std::ifstream instance(myfile);
    int i = 0;

    if (instance.is_open()) {
        while ( getline (instance,line) ) {
            if(i == 0){
                _tailleTerrain = extractIntegerFromString(line,0);
            } else if(i == 1){

            } else {
                Building building(_buildings.size()+1,extractIntegerFromString(line,1),extractIntegerFromString(line,0));
                _buildings.push_back(building);
            }
            i++;
        }
        instance.close();
    } else std::cout << "Unable to open file";
}



void Game::placementDefault() {
    int i = 0;
    for (int j = 0; j < _terrain.size(); j++) {
        for (int k = 0; k < _terrain.size(); k++) {
            // Ordre des buildings
            if (i < _buildings.size()) {
                // Si il n'y a pas de la place pour poser le nouveau building on fait une rotation
                if(placementBusyForBuilding(_buildings.at(i),j,k)){
                    _buildings.at(i).rotation();
                }
                // Si il y a de la place pour poser le nouveau building et si l'on peut poser une route
                if(!placementBusyForBuilding(_buildings.at(i), j, k) ) {
                // On pose le building
                placement(_buildings.at(i), j, k);
                // On passe le building en placer.
                _buildings.at(i).setPlacement(true);

                i++;
                }
            }
        }
    }
}

// Placement du bâtiment  et de la route
void Game::placement(Building building, int posX,int posY){
    for (int i = posX; i < posX + building.getX(); i++) {
        for (int j = posY; j < posY + building.getY(); j++) {
            _terrain[i][j] = building.getId();
        }
    }

    if(building.getId() != 1) {
        for (int i = posX; i < posX + building.getX(); i++) {
            for (int j = posY; j < posY + building.getY(); j++) {
                // Si X n'est pas à la première ligne
                // Test case adjacente en haut
                if (i > 0) {
                    if (_terrain[i - 1][j] == 0) {
                        // Test case adjacente en bas pour savoir si il existe déjà une route
                        if(i < _tailleTerrain-1) {
                            if (_terrain[i][j - 1] != -1) {
                                _terrain[i - 1][j] = -1;
                            }
                        } else {
                            _terrain[i - 1][j] = -1;
                        }
                    }
                }
                // Si Y n'est pas à la première colonne de gauche
                // Test case adjacente à gauche
                if (j > 0) {
                    if (_terrain[i][j - 1] == 0) {
                        // Test case adjacente à droite pour savoir si il existe déjà une route
                        if(j < _tailleTerrain-1) {
                            if (_terrain[i - 1][j] != -1) {
                                _terrain[i][j - 1] = -1;
                            }
                        } else {
                            _terrain[i][j - 1] = -1;
                        }
                    }
                }
                // Si X n'est pas à la dernière ligne
                // Test case adjacente en bas
                if (i < _tailleTerrain-1) {
                    if (_terrain[i + 1][j] == 0) {
                        // Test case adjacente en haut pour savoir si il existe déjà une route
                        if(i > 0) {
                            if (_terrain[i + 1][j] != -1) {
                                _terrain[i + 1][j] = -1;
                            }
                        } else {
                          _terrain[i + 1][j] = -1;
                        }
                    }
                }
                // Si Y n'est pas à la dernière colonne de droite
                // Test case adjacente à droite
                if (j < _tailleTerrain-1) {
                    if (_terrain[i][j + 1] == 0) {
                        // Test case adjacente à gauche pour savoir si il existe déjà une route
                        if (j > 0) {
                            if (_terrain[i][j-1] != -1) {
                                _terrain[i][j + 1] = -1;
                            }
                        } else {
                            _terrain[i][j + 1] = -1;
                        }
                    }
                }
            }
        }
    }
}

// Tester si l'on peut mettre un bâtiment
bool Game::placementBusyForBuilding(Building building, int posX,int posY) {
    bool placement = false;

    if(posX+building.getX() <= _terrain.size() && posY+building.getY() <= _terrain.size()) {

        for (int i = posX; i < posX + building.getX(); i++) {
            for (int j = posY; j < posY + building.getY(); j++) {
                if (busy(i, j)) {
                    placement = true;
                }
            }
        }

    } else {
        placement = true;
    }

    return placement;
}

void Game::eval() {
    int eval = 0;
    for(int i = 0; i<_terrain.size(); i++){
        for(int j = 0; j<_terrain.size(); j++){
            if(_terrain[i][j] != 0 && _terrain[i][j] != -1) {
                eval++;
            }
        }
    }
    _fitness = eval;
}

void Game::variantesGloutons(int variante) {
    std::vector<Building> from;

    from.push_back(_buildings.at(0));
    _buildings.erase (_buildings.begin());

    if(variante == 1){
        for(int i = 0 ; i < _buildings.size()+1; i++) {
            int indexMaxBuilding = maxAire(_buildings);
            from.push_back(_buildings.at(indexMaxBuilding));
            _buildings.erase(_buildings.begin()+indexMaxBuilding);
            i = 0;
        }
        _buildings.assign(from.begin(), from.end());
    } else if(variante == 2){
        for(int i = 0 ; i < _buildings.size()+1; i ++) {
            int indexMaxBuilding = maxEncombrement(_buildings);
            from.push_back(_buildings.at(indexMaxBuilding));
            _buildings.erase(_buildings.begin()+indexMaxBuilding);
            i = 0;
        }
        _buildings.assign(from.begin(), from.end());
    } else if (variante == 3){
        for(int i = 0 ; i < _buildings.size()+1; i ++) {
            srand ( time(NULL) );
            int indexRandomBuilding = rand() % _buildings.size() + 0;
            from.push_back(_buildings.at(indexRandomBuilding));
            _buildings.erase(_buildings.begin()+indexRandomBuilding);
            i = 0;
        }
        _buildings.assign(from.begin(), from.end());
    } else if (variante == 4){
        for(int i = 0 ; i < _buildings.size()+1; i ++) {
            int indexMinBuilding = minAire(_buildings);
            from.push_back(_buildings.at(indexMinBuilding));
            _buildings.erase(_buildings.begin()+indexMinBuilding);
            i = 0;
        }
        _buildings.assign(from.begin(), from.end());
    } else {
        std::cout << "Erreur de choix de variante" << std::endl;
    }
}

int Game::maxAire(std::vector<Building> buildings) {
    int max = 0;

    for(int i = 0; i < buildings.size(); i++){
        if(buildings.at(i).getX()*buildings.at(i).getY() > buildings.at(max).getX()*buildings.at(max).getY()){
            max = i;
        }
    }

    return max;
}

int Game::maxEncombrement(std::vector<Building> buildings) {
    int max = 0;

    for(int i = 0; i < buildings.size(); i++){
        if(buildings.at(i).getX()+buildings.at(i).getY() > buildings.at(max).getX()+buildings.at(max).getY()){
            max = i;
        }
    }

    return max;
}

int Game::minAire(std::vector<Building> buildings) {
    int min = 0;

    for(int i = 0; i < buildings.size(); i++){
        if(buildings.at(i).getX()*buildings.at(i).getY() < buildings.at(min).getX()*buildings.at(min).getY()){
            min = i;
        }
    }

    return min;
}