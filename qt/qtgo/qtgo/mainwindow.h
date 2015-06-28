#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFontDatabase>

#include "Global.h"

class DrawAreaWidget;
class GoTable;
class GameSettings;
class RoundInfo;
class MiniGameSettings;
class ConfirmMoveDialog;
class QToolButton;

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

    void runDebug();

signals:
    void programSettingsChanged();

private:
    void createMiniInterface();


private:
    bool minimalInterface = false;
    GameSettings* gameSettingsWidget = NULL;
    MiniGameSettings* miniGameSettings = NULL;
    ConfirmMoveDialog* confirmMoveDialog = NULL;
    Ui::MainWindow *ui;
    DrawAreaWidget* drawArea;
    GoTable* table;
    RoundInfo* roundInfo = NULL;
    QFontDatabase fontDatabase;
};

#endif // MAINWINDOW_H
