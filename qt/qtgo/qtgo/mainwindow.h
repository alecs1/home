#pragma once

#include <QMainWindow>
#include <QFontDatabase>
#include <QTimer>

#include "Global.h"

class DrawAreaWidget;
class GoTable;
class RoundInfo;
class GameSettings;
class MiniGameSettings;
class PeerChooser;
class ConfirmMoveDialog;
class QToolButton;
class BTServer;
class ConnMan;


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

    int connectBT();
    void connectTCP();
    void showBTChat();
    void showAbout();
    void showHelp();
    void showSettings();

    void mainLoop();

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

    BTServer* btServer = nullptr;
    ConnMan* connMan = nullptr;
};
