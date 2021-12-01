#include "../models/gameModel.h"

gameModel::gameModel(QObject *parent) :
    QAbstractListModel(parent)
{
    connect(&game_, &Game::gameChangedAfterMovement, this, &gameModel::onDataChanged);    
    connect(&game_, &Game::scoreChanged, this, &gameModel::scoreChanged);
    connect(&game_, &Game::winOrEndChanged, this, &gameModel::winOrEndChanged);
}

int gameModel::rowCount(const QModelIndex &parent) const {
    Q_UNUSED(parent)

    return gridSize * gridSize;
}

QVariant gameModel::data(const QModelIndex &index, int role) const {
    int x, y;
    if (index.row() < 0 || index.row() >= gridSize * gridSize) {
        return QVariant();
    }
    if (role == Qt::DisplayRole) {
        x = index.row() % gridSize;
        y = index.row() / gridSize;
        QString str = QString::number(game_.getPOS(x, y));
        return str;
    }
    return QVariant();
}

void gameModel::onDataChanged() {
    emit dataChanged(createIndex(0, 0), createIndex(rowCount() - 1, 0));
}

void gameModel::moveUp() {
    game_.moveUp();
}

void gameModel::moveRight() {
    game_.moveRight();
}

void gameModel::moveDown() {
    game_.moveDown();
}

void gameModel::moveLeft() {
    game_.moveLeft();
}

void gameModel::restart() {
    game_.restart();
}

int gameModel::score() const {
    return game_.score();
}

bool gameModel::end() {
    return game_.end();
}

bool gameModel::win() {
    return game_.win();
}
