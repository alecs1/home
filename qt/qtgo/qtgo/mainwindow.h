#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

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


private:
    Ui::MainWindow *ui;
    GoTable* table;
};

#endif // MAINWINDOW_H
