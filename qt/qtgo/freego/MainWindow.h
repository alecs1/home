#pragma once

#include <QMainWindow>
#include <QFontDatabase>
#include <QTimer>

#include "Global.h"

#include "network/ConnMan.h"

#include "network/ProtoJson.h" //TODO - temporary.

class DrawAreaWidget;
class GoTable;
class RoundInfo;
class GameSettings;
class MiniGameSettings;
class PeerChooser;
class ConfirmMoveDialog;
class QToolButton;
class BTServer;

namespace notifications {
class DockedNotif;
}

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

public slots:
    void saveGame();
    void loadGame();
    void notifyReloadProgramSettings();
    void setMinimalInterface();
    void transitionToMinDone();
    void setFullInterface();
    void restoreFullInterface();
    void transitionToFullDone();
    void showConfirmDialog(bool show, int colour);
    void confirmDialogDone(int confirmed);
    void setGameState(GameState state);
    void onMovePlayed(int row, int col);

    int connectBT();
    void connectTCP();
    void showBTChat();
    void showAbout();
    void showHelp();
    void showSettings();

    void mainLoop();

    void onConnStateChanged(ConnMan::ConnState state, bool initiator, ConnMan::ConnType connType);

    void onRemoteMessage(const ProtoJson::Msg& msg);

signals:
    void programSettingsChanged();

private:
    void setupGameSettings();
    void createMiniInterface();


private:
    bool minimalInterface = false;
    GameSettings* gameSettingsWidget = nullptr;
    MiniGameSettings* miniGameSettings = nullptr;
    PeerChooser* peerChooser = nullptr;
    ConfirmMoveDialog* confirmMoveDialog = nullptr;
    Ui::MainWindow *ui;
    DrawAreaWidget* drawArea;
    GoTable* table;
    RoundInfo* roundInfo = nullptr;
    QTimer mainLoopTimer;
    QFontDatabase fontDatabase;

    //transient dialog children
    notifications::DockedNotif* makeSettingsDock = nullptr;

    ConnMan* connMan = nullptr;

    ProtoJson::Msg activeMessage; //last message sent for which we expect a reply
};


