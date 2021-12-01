//
// Created by nfourny on 28/12/2020.
//

#ifndef PROJET_GLOUTON_2020_BUILDING_H
#define PROJET_GLOUTON_2020_BUILDING_H
#include <string>

class Building {
private:
    int _id;
    int _x;
    int _y;
    bool _placement;
    std::string _color;

public:
    Building(int id, int x, int y);
    int getId();
    int getX();
    int getY();
    int getPlacement();

    void setPlacement(bool placement);
    void setX(int x);
    void setY(int y);

    void rotation();
};



#endif //PROJET_GLOUTON_2020_BUILDING_H
