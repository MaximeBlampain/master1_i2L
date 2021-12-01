#include <QGuiApplication>
#include <QCoreApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include<iostream>
#include<fstream>

#include "models/gameModel.h"
#include "models/game.h"
#include "models/gameIARandom.h"
#include "models/gameIAHillClimbing.h"
#include "models/gameIAMontecarlo.h"
#include "models/gameIAExpectImax.h"
#include "models/gameIAMiniMax.h"
#include "models/gameIAMc.h"

int main(int argc, char *argv[])
{
    /* Normal
    QGuiApplication app(argc, argv);

    QQmlApplicationEngine engine;
    gameModel game;

    engine.rootContext()->setContextProperty("game", &game);
    engine.load(QUrl(QStringLiteral("qrc:///qml/Main.qml")));
    return app.exec();
    */

    /* IA */
    for(int i = 0 ; i < 1 ; i++) {
        //GameIARandom gameIARandom("C:/Users/33681/Documents/Applications/2048-project/data/randomPerformance.csv");
        //GameIAHillClimbing gameIAHillClimbing("C:/Users/33681/Documents/Applications/2048-project/data/HillClimbingPerformance.csv");
        //GameIAMontecarlo gameIAMontecarlo("C:/Users/33681/Documents/Applications/2048-project/data/MonteCarloPerformance.csv");
        //GameIAExpectImax gameIAExpectImax("C:/Users/33681/Documents/Applications/2048-project/data/ExpectImaxPerformance.csv");
        //GameIAMiniMax gameIAMiniMax("/Users/nfourny/QT-PROJECT/2048-project/data/MiniMaxPerformance.csv");
        GameIAMc gameIAMc("/Users/nfourny/QT-PROJECT/2048-project/data/MonteCarloMCTSPerformance.csv");
    }

    exit(0);
}
