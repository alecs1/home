#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "GoTable.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    /*
    The kind of contortion done here:
    1. create table as top window
    2. create QWidget window container to contain the table window
    3. create new layout, set it to govern the central widget of main window and then give it the window container
    */

    GoTable* table = new GoTable(0);
    QWidget * container = createWindowContainer(table, this);
    ui->centralWidget->setLayout(ui->gridLayout);
    ui->gridLayout->addWidget(container);
}

MainWindow::~MainWindow()
{
    delete ui;
}
