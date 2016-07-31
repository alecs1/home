#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFontDatabase>

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

//enum class ComputingPlatform : uint8_t {
//    DesktopLinux = 0,
//    DesktopWindows,
//    Android
//};


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
    QFontDatabase fontDatabase;

    BTServer* btServer = nullptr;
    ConnMan* connMan = nullptr;
};

#endif // MAINWINDOW_H
