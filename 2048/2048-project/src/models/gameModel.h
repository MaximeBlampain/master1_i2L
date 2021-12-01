#ifndef gameModel_H
#define gameModel_H

#include <QAbstractListModel>
#include "game.h"

class gameModel : public QAbstractListModel
{
    Q_OBJECT
    Q_PROPERTY(int score READ score NOTIFY scoreChanged)
    Q_PROPERTY(bool end READ end NOTIFY winOrEndChanged)
    Q_PROPERTY(bool win READ win NOTIFY winOrEndChanged)

private:
    Game game_;

public:
    explicit gameModel(QObject *parent = 0);

    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

    bool end();
    bool win();
    int score() const;

    Q_INVOKABLE void moveUp();
    Q_INVOKABLE void moveRight();
    Q_INVOKABLE void moveDown();
    Q_INVOKABLE void moveLeft();
    Q_INVOKABLE void restart();

signals:
    void scoreChanged();
    void winOrEndChanged();


public slots:
    void onDataChanged();

};

#endif // gameModel_H
