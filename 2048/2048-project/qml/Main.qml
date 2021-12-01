import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Dialogs 1.1
import QtQuick.Window 2.1

ApplicationWindow {
    id: mainWindow
    visible: true
    width: 510
    height: 700
    minimumWidth: 510
    minimumHeight: 700
    maximumWidth: 510
    maximumHeight: 700
    title: qsTr("2048");

    x: (Screen.width - width) / 2
    y: (Screen.height - height) / 2

    menuBar: MenuBar {
        Menu {
            title: qsTr("Fichier")
            MenuItem {
                text: qsTr("Nouvelle partie")
                shortcut: "Ctrl+N"
                onTriggered: game.restart()
            }
        }

        Menu {
            id: helpMenu
            title: qsTr("Aide")
            MenuItem {
                text: qsTr("A propos de")
                onTriggered: aboutDialog.open();
            }
        }
    }


    Item {
        id: helper
        focus: false
        property var myColors: {
            "bglight": "#FAF8EF",
            "bggray": Qt.rgba(238/255, 228/255, 218/255, 0.35),
            "bgdark": "#BBADA0",
            "fglight": "#EEE4DA",
            "fgdark": "#776E62",
            "bgbutton": "#8F7A66",
            "fgbutton": "#F9F6F2"
        }
    }
    color: helper.myColors.bglight

    Item {
        id: item1
        width: 500
        height: 670
        anchors.centerIn: parent
        state: game && game.end ? deadMessage.open() : game && game.win ? winMessage.open() : null

        focus: true
        Keys.onPressed: {
          switch (event.key) {
          case Qt.Key_Up:
              game.moveUp();
              break;
          case Qt.Key_Right:
              game.moveRight();
              break;
          case Qt.Key_Down:
              game.moveDown();
              break;
          case Qt.Key_Left:
              game.moveLeft();
              break;
          }
        }

        MouseArea {
            anchors.fill: parent
            onClicked: parent.forceActiveFocus()

            Image {
                id: image
                x: 0
                y: 0
                width: 142
                height: 119
                source: "../res/imgs/2048_logo.svg"
                fillMode: Image.PreserveAspectFit
            }
        }

        Row {
            x: 185
            y: 0
            width: 315
            height: 55
            opacity: 0.9
            spacing: 15
            layoutDirection: Qt.LeftToRight
            Repeater {
                id: scoreBoard
                model: 2
                Rectangle {
                    width: 150
                    height: 55
                    radius: 3
                    color: helper.myColors.bgdark
                    Text {
                        text: (index == 0) ? qsTr("Score") : qsTr("Meilleur score")
                        anchors.horizontalCenter: parent.horizontalCenter
                        y: 6
                        font.pixelSize: 13
                        color: helper.myColors.fglight
                    }
                    Text {
                        text: game && game.score
                        anchors.horizontalCenter: parent.horizontalCenter
                        y: 22
                        font.pixelSize: 25
                        font.bold: true
                        color: "white"
                    }
                }
            }

            Text {
                id: addScoreText
                font.pixelSize: 25
                font.bold: true
                color: Qt.rgba(119/255, 110/255, 101/255, 0.9);
                anchors.horizontalCenter: parent.horizontalCenter

                property bool runAddScore: false
                property real yfrom: 0
                property real yto: -(parent.y + parent.height)
                property int addScoreAnimTime: 600

                ParallelAnimation {
                    id: addScoreAnim
                    running: false

                    NumberAnimation {
                        target: addScoreText
                        property: "y"
                        from: addScoreText.yfrom
                        to: addScoreText.yto
                        duration: addScoreText.addScoreAnimTime

                    }
                    NumberAnimation {
                        target: addScoreText
                        property: "opacity"
                        from: 1
                        to: 0
                        duration: addScoreText.addScoreAnimTime
                    }
                }
            }
        }

        Button {
            x: 185
            width: 315
            height: 56
            y: 61
            anchors.right: parent.right
            checkable: false
            anchors.rightMargin: 0
            Component.onCompleted: __behavior.cursorShape = Qt.PointingHandCursor

            style: ButtonStyle {
                background: Rectangle {
                    color: helper.myColors.bgbutton
                    radius: 3
                    Text{
                        anchors.centerIn: parent
                        text: qsTr("Nouvelle partie")
                        color: helper.myColors.fgbutton
                        font.pixelSize: 18
                        font.bold: true
                    }
                }
            }
            onClicked: game.restart()
        }

        Rectangle {
            id: mainGame
            x: -5
            y: 170

            function step() {
                return Math.min(mainGame.width, mainGame.height) / 33;
            }

            width: 510
            height: 514
            color: "#baaa9e"

            Grid {
                y: mainGame.step()
                anchors.horizontalCenter: parent.horizontalCenter
                rows: 4
                columns: 4
                spacing: mainGame.step()

                Repeater {
                    id: cells
                    model: game
                    delegate: Tile {
                        value: display
                        tileWidth: mainGame.step() * 7
                    }
                }
            }
        }

        MessageDialog {
            id: aboutDialog
            title: qsTr("A propos du jeu 2048")
            text: qsTr("<p style='font-weight: bold; font-size: 24px'>Jeu 2048 - Version 1.0</p>
                        <p>Crée par FOURNY Nicolas & BLAMPAIN Maxime</p>
                        <p>Projet Master 1 ULCO</p>")
            standardButtons: StandardButton.Ok
        }

        MessageDialog {
            id: deadMessage
            title: qsTr("Perdu !")
            text: qsTr("Vous avez perdu ! Voulez vous rejouer ?")
            standardButtons: StandardButton.Yes | StandardButton.No
            onYes: {
                game.restart()
                close()
            }
            onNo: Qt.quit()
        }

        MessageDialog {
            id: winMessage
            title: qsTr("Gagné !")
            text: qsTr("Vous avez gagné ! Voulez-vous rejouer ?")
            standardButtons: StandardButton.Yes | StandardButton.No
            onYes: {
                game.restart()
                close()
            }
            onNo: Qt.quit()
        }

    }
}


/*##^##
Designer {
    D{i:0;formeditorZoom:0.6600000262260437}
}
##^##*/
