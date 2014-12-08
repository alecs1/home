#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "GoTable.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    GoTable* table = new GoTable(0);
    ui->centralWidget->setLayout(ui->gridLayout);
    ui->gridLayout->addWidget(table);
}

MainWindow::~MainWindow()
{
    delete ui;
}
