//
// Created by nfourny on 28/12/2020.
//

#include "Building.h"

Building::Building(int id, int x, int y) {
    _id = id;
    _x = x;
    _y = y;
    _placement = false;
}

int Building::getId() {
    return _id;
}

int Building::getX() {
    return _x;
}

int Building::getY() {
    return _y;
}

int Building::getPlacement() {
    return _placement;
}

void Building::setPlacement(bool placement) {
    _placement = placement;
}

void Building::rotation() {
    int tmpX = _x;
    setX(_y);
    setY(tmpX);
}

void Building::setX(int x) {
    _x = x;
}

void Building::setY(int y) {
    _y = y;
}
