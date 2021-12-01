#include "../models/game.h"
#include "math.h"
#include <iostream>
#include "qvector.h"
#include <QTime>
#include <QCoreApplication>

using namespace std;

Game::Game(QObject *parent) : QObject(parent) {
    for(int i = 0 ; i < gridSize ; i++){
        game_.resize(gridSize);
        for(int j = 0 ; j < gridSize ; j++){
            game_[i].resize(gridSize);
            game_[i][j] = 0;
        }
    }
    init();
    playerTurn_ = true;
}

Game::Game(std::string path) {
    path_ = path;
    for(int i = 0 ; i < gridSize ; i++){
        game_.resize(gridSize);
        for(int j = 0 ; j < gridSize ; j++){
            game_[i].resize(gridSize);
            game_[i][j] = 0;
        }
    }
    init();
    playerTurn_ = true;
}

Game::Game(std::vector<std::vector<int>> game, int score) {
   game_ = game;
   score_ = score;
}

Game::Game(std::vector<std::vector<int>> game, int score,int bestTile, bool playerTurn) {
   game_ = game;
   score_ = score;
   playerTurn_ = playerTurn;
   bestTile_ = bestTile;
}

void Game::init() {
    // Initialise le score et la meilleure tuile à 0
    score_ = 0;
    bestTile_ = 0;

    // On ajoute deux tuiles au hasard pour le début
    addRandomTile();
    addRandomTile();
}

void Game::restart(){
    for(int i = 0 ; i < gridSize ; i++){
        for(int j = 0 ; j < gridSize ; j++){
            game_[i][j] = 0;
        }
    }
    init();
    emit gameChangedAfterMovement();
    emit scoreChanged();
    //run();
}

std::vector<int> Game::availableCells() {
    // Un vector des cellules disponibles
    std::vector<int> cellsAvailable;

    // On push les cellules libres
    for (int i = 0; i < gridSize; i++) {
        for (int j = 0; j < gridSize; j++) {
            if (game_[i][j] == 0) {
                cellsAvailable.push_back(i * gridSize + j);
            }
        }
    }
    return cellsAvailable;
}

void Game::addRandomTile() {
    // Un vector des cellules disponibles
    QVector<int> _cellsAvailable;

    // On push les cellules libres
    for (int i = 0; i < gridSize; i++) {
        for (int j = 0; j < gridSize; j++) {
            if (game_[i][j] == 0) {
                _cellsAvailable.push_back(i * gridSize + j);
            }
        }
    }

    // On choisit une cellule random d'après le vector cellules disponibles
    int randomCellId = _cellsAvailable[floor(rand() % _cellsAvailable.size())];

    // On récupère les coordonnées de la cellule libre choisis aléatoirement
    QVector<int> _randomTileCoeffStart;

    _randomTileCoeffStart.push_back(floor(randomCellId / gridSize));
    _randomTileCoeffStart.push_back(randomCellId % gridSize);

    std::srand(static_cast<unsigned int>(std::time(nullptr)));
    float myRandom = std::rand() / RAND_MAX;
    // On passe la tuile à 2 ou à 4
    if(myRandom < 0.90){
       game_[_randomTileCoeffStart[0]][_randomTileCoeffStart[1]] = 2;
    } else{
       game_[_randomTileCoeffStart[0]][_randomTileCoeffStart[1]] = 4;
    }

}

bool Game::move(Directions direction) {
    bool success = false;

    for(int i = Up; i < direction; i++) {
        rotate();
    }
    success = moveTilesTop();
    for(int i = direction; i <= Right; i++) {
        rotate();
    }
    if (success) {
        addRandomTile();
        emit gameChangedAfterMovement();
    }

    if(win() || end()) {
        emit winOrEndChanged();
        return false;
    }

    return success;
}

bool Game::moveWithoutAddRandomTiles(Directions direction) {
    bool success = false;

    for(int i = Up; i < direction; i++) {
        rotate();
    }
    success = moveTilesTop();
    for(int i = direction; i <= Right; i++) {
        rotate();
    }
    if (success) {
        emit gameChangedAfterMovement();
    }

    if(win() || end()) {
        emit winOrEndChanged();
        return false;
    }

    return success;
}

bool Game::moveTilesTop() {
    bool success = false;

    if (win() || end()) {
        return false;
    }

    for (int x = 0; x < gridSize; x++) {
        success |= merge(game_[x]);
    }
    return success;
}


// Fusion des tuiles
bool Game::merge(std::vector<int> & array) {
    bool success = false;
    int pos, target, stop = 0;
    int mergeValue;

    for (pos = 0; pos < gridSize; pos++) {
        if (array[pos] != 0) {
            target = findTarget(array, pos, stop);
            if (target != pos) {

                if (array[target] != 0) {
                    stop = target + 1;
                    mergeValue = array[target] + array[pos];
                    score_ += mergeValue;
                    emit scoreChanged();
                    bestTile_ = max(bestTile_, mergeValue);
                }
                array[target] += array[pos];
                array[pos] = 0;
                success = true;
            }
        }
    }

    return success;
}

// Trouver la pos de la tuile
int Game::findTarget(std::vector<int> & array, const int pos, const int stop) const {
    int target;
    if (pos == 0) {
        return pos;
    }
    for (target = pos - 1; target >= 0; target--) {
        if (array[target] != 0) {
            if (array[target] != array[pos]) {
                return target + 1;
            }
            return target;
        } else {
            if (target == stop) {
                return target;
            }
        }
    }
    return pos;
}

// Trouve si deux tuiles peuvent fusionner
bool Game::findPairNextTo() const {
    bool pair = false;
    int x, y;
    for (x = 0; x < gridSize; x++) {
        for (y = 0; y < gridSize - 1; y++) {
            if (game_[x][y] == game_[x][y+1]) {
                return true;
            }
        }
    }

    return pair;
}

// Compte le nombres de cellules vides.
int Game::countEmptyTile() const {
    int x, y, count = 0;
    for (x = 0; x < gridSize; x++) {
        for (y = 0; y < gridSize; y++) {
            if (game_[x][y] == 0) {
                count++;
            }
        }
    }
    return count;
}

// Rotation
void Game::rotate() {
    int i, j, tmp;
    for (i = 0; i < gridSize / 2; i++) {
        for (j = i; j < gridSize - i - 1; j++) {
            tmp = game_[i][j];
            game_[i][j] = game_[j][gridSize-i-1];
            game_[j][gridSize-i-1] = game_[gridSize-i-1][gridSize-j-1];
            game_[gridSize-i-1][gridSize-j-1] = game_[gridSize-j-1][i];
            game_[gridSize-j-1][i] = tmp;
        }
    }
}

bool Game::end() {
    bool end = true;

    // Si il y a des cellules libres alors on peut continuer à jouer
    if (countEmptyTile() > 0) {
        return false;
    }
    // Si il y a de deux cellules à côté qui peuvent fusionner alors on peut continuer à jouer
    if (findPairNextTo()) {
        return false;
    }

    // Rotation
    rotate();

    if (findPairNextTo()) {
        end = false;
    }
    rotate();
    rotate();
    rotate();
    return end;
}

int Game::getPOS(const int x, const int y) const {
    return game_[x][y];
}

int Game::score() const {
    return score_;
}

int Game::bestTile() const {
    return bestTile_;
}

bool Game::moveUp() {
    return move(Up);
}

bool Game::moveLeft() {
    return move(Left);
}

bool Game::moveDown() {
    return move(Down);
}

bool Game::moveRight() {
    return move(Right);
}

bool Game::win() {
    return bestTile_ == win_value;
}

void Game::addComputerTile(int x, int y){
    std::srand(static_cast<unsigned int>(std::time(nullptr)));
    float myRandom = std::rand() / RAND_MAX;
    // On passe la tuile à 2 ou à 4
    if(myRandom < 0.90){
        game_[x][y] = 2;
    } else{
        game_[x][y] = 4;
    }
}

void Game::delay(int delay) {
    QTime dieTime= QTime::currentTime().addMSecs(delay);
    while (QTime::currentTime() < dieTime)
        QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
}

std::vector<Directions> Game::availableMove(){
    Game gameCopy(game_,score());
    std::vector<Directions> availableMove;
    std::vector<std::vector<int>> defaultArray = gameCopy.game_;

    gameCopy.move(Up);
    if(game_ != gameCopy.game_){
        availableMove.push_back(Up);
    }
    gameCopy.game_ = defaultArray;

    gameCopy.move(Left);
    if(game_ != gameCopy.game_){
        availableMove.push_back(Left);
    }
    gameCopy.game_ = defaultArray;

    gameCopy.move(Down);
    if(game_ != gameCopy.game_){
        availableMove.push_back(Down);
    }
    gameCopy.game_ = defaultArray;

    gameCopy.move(Right);
    if(game_ != gameCopy.game_){
        availableMove.push_back(Right);
    }
    gameCopy.game_ = defaultArray;

    return availableMove;
}

std::vector<std::vector<int>> & Game::getGame() {
    return game_;
}

Directions Game::nextMove(int index){
    if(index==0)
        return Up;

    if(index==1)
        return Left;

    if(index==2)
        return Down;

    if(index==3)
        return Right;
}

void Game::export_csv(){
    std::ofstream myfile;
    myfile.open(path_, std::ofstream::out | std::ofstream::app);
    myfile << score_ << ";" << bestTile_ << "\n";
    myfile.close();
}
