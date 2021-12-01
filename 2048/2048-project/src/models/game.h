#ifndef game_H
#define game_H

#include <QObject>
#include <QStringList>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <random>

#include "MCTnode.h"

static constexpr int gridSize = 4;
static constexpr int win_value = 10000000;
enum Directions {
    Up = 0,
    Left,
    Down,
    Right
};

static constexpr int MAX_TILE_CREDIT = 10e4;
static constexpr int WEIGHT_MATRIX[4][4] = {2048, 1024, 64, 32,512, 128, 16, 2,256, 8, 2, 1,4, 2, 1, 1};

class Game : public QObject {
    Q_OBJECT

public:
    std::vector<std::vector<int>> game_;
    std::string path_;
    int score_;
    int bestTile_;
    bool playerTurn_;

    explicit Game(QObject *parent = 0);
    explicit Game(std::string path);
    explicit Game(std::vector<std::vector<int>> s, int score);
    explicit Game(std::vector<std::vector<int>> s, int score, int bestTile, bool playerTurn);

    std::vector<std::vector<int>> & getGame();

    void addComputerTile(int x, int y);

    void init();
    void restart();
    void addRandomTile();

    bool move(Directions direction);
    bool moveWithoutAddRandomTiles(Directions direction);

    bool moveTilesTop();

    void rotate();
    bool merge(std::vector<int> & array);
    int findTarget(std::vector<int> & array, const int pos, const int stop) const;
    bool findPairNextTo() const;

    int countEmptyTile() const;

    bool moveUp();
    bool moveRight();
    bool moveDown();
    bool moveLeft();

    int getPOS(const int x, const int y) const;
    int score() const;
    int bestTile() const;

    bool end();
    bool win();

    void delay(int delay);

    std::vector<Directions> availableMove();
    std::vector<int> availableCells();
    Directions nextMove(int index);

    void export_csv();

signals:
    void gameChangedAfterMovement();
    void scoreChanged();
    void winOrEndChanged();

public slots:
};

#endif // game_H
