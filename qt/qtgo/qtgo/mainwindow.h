#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFontDatabase>

class DrawAreaWidget;
class GoTable;

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

signals:
    void programSettingsChanged();


private:
    Ui::MainWindow *ui;
    DrawAreaWidget* drawArea;
    GoTable* table;
    QFontDatabase fontDatabase;
};

#endif // MAINWINDOW_H
