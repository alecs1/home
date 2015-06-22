#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFontDatabase>

class DrawAreaWidget;
class GoTable;
class GameSettings;
class RoundInfo;
class MiniGameSettings;

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
    void transitionDone();

signals:
    void programSettingsChanged();


private:
    GameSettings* gameSettingsWidget = NULL;
    MiniGameSettings* miniGameSettings = NULL;
    Ui::MainWindow *ui;
    DrawAreaWidget* drawArea;
    GoTable* table;
    RoundInfo* roundInfo = NULL;
    QFontDatabase fontDatabase;
};

#endif // MAINWINDOW_H
