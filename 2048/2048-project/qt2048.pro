TEMPLATE = app

CONFIG+=c++14 console

QT += qml quick core

# Additional import path used to resolve QML modules in Qt Creator's code model
QML_IMPORT_PATH =

# Default rules for deployment.
include(deployment.pri)

SOURCES += \
    src/exe/game.cpp \
    src/exe/gameModel.cpp \
    src/main.cpp \

HEADERS += \
    src/models/MCTnode.h \
    src/models/game.h \
    src/models/gameIAExpectImax.h \
    src/models/gameIAMc.h \
    src/models/gameIAMiniMax.h \
    src/models/gameIAMontecarlo.h \
    src/models/gameModel.h \
    src/models/gameIARandom.h \
    src/models/gameIAHillClimbing.h \

RESOURCES += \
    resources.qrc \

QMAKE_LFLAGS += -v
