#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "GoTable.h"
#include "GameSettings.h"

//ComputingPlatform PlatformType()
//{
//    if (d)
//   return ComputingPlatform::DesktopLinux;
//}

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    GoTable* table = new GoTable(this);
    ui->centralWidget->setLayout(ui->gridLayout);

    ui->gridLayout->addWidget(table, 0, 0);
    ui->gridLayout->setColumnStretch(0, 5);
    ui->gridLayout->setColumnMinimumWidth(0, table->minimumWidth());
    ui->gridLayout->setRowMinimumHeight(0, table->minimumHeight());

    //TODO: this type of size computation is a poor hack, instead my table should provide a good minimum hint.
    int minWidth = table->minimumSize().width();
    int minHeight = table->minimumSize().height();


    #if defined(Q_OS_LINUX) || defined(Q_OS_WIN32) || defined(Q_OS_WIN64)
    GameSettings* settings = new GameSettings(this);
    ui->gridLayout->addWidget(settings, 0, 1);
    ui->gridLayout->setColumnStretch(1, 1);
    QObject::connect(settings, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    QObject::connect(table, SIGNAL(GameStateChanged(GameState)), settings, SLOT(setGameState(GameState)));
    minWidth += settings->sizeHint().width();
    minWidth *= 1.1;
    minHeight *= 1.27;
    #endif

    printf("%s - computed min sizes: %dx%d\n", __func__, minWidth, minHeight);
    setMinimumSize(minWidth, minHeight);

}

MainWindow::~MainWindow()
{
    delete ui;
}