#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "GoTable.h"
#include "GameSettings.h"

#include "Global.h"

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

    #if defined(Q_OS_ANDROID)
    QList<QToolBar *> toolbars = findChildren<QToolBar *>();
    for (auto* toolbar : toolbars) {
        printf("%s - hiding toolbar %p\n", __func__, toolbar);
        toolbar->hide();
    }
    statusBar()->hide();
    #else
    QList<QToolBar *> toolbars = findChildren<QToolBar *>();
    for (auto* toolbar : toolbars) {
        printf("%s - hiding toolbar %p\n", __func__, toolbar);
        toolbar->hide();
    }
    statusBar()->hide();
    #endif

    #if defined(Q_OS_ANDROID)
    printf("%s - setting fullscreen for Android - TODO: does not work!!!\n", __func__);
    showFullScreen();
    printf("%s - is fullscreen=%d\n", __func__, isFullScreen());
    #endif

    GoTable* table = new GoTable(this);
    ui->centralWidget->setLayout(ui->gridLayout);

    ui->gridLayout->addWidget(table, 0, 0);
    ui->gridLayout->setColumnStretch(0, 5);
    ui->gridLayout->setColumnMinimumWidth(0, table->minimumWidth());
    ui->gridLayout->setRowMinimumHeight(0, table->minimumHeight());

    //TODO: this type of size computation is a poor hack, instead my table should provide a good minimum hint.
    int minWidth = table->minimumSize().width();
    int minHeight = table->minimumSize().height();


    //seems Q_OS_LINUX also applies to Android
    GameSettings* settings = new GameSettings(this);
    ui->gridLayout->addWidget(settings, 0, 1);
    ui->gridLayout->setColumnStretch(1, 1);
    QObject::connect(settings, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    QObject::connect(settings, SIGNAL(doEstimateScore(bool)), table, SLOT(activateEstimatingScore(bool)));
    QObject::connect(table, SIGNAL(gameStateChanged(GameState)), settings, SLOT(setGameState(GameState)));
    QObject::connect(table, SIGNAL(estimateScoreChanged(float)), settings, SLOT(setScoreEstimate(float)));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int,PlayerType)), settings, SLOT(setCurrentPlayer(int,PlayerType)));
    QObject::connect(table, SIGNAL(askUserConfirmation(bool)), settings, SLOT(showConfirmButton(bool)));
    QObject::connect(settings, SIGNAL(userConfirmedMove(int)), table, SLOT(userConfirmedMove(int)));
    minWidth += settings->sizeHint().width();
    minWidth *= 1.1;
    minHeight *= 1.27;


    printf("%s - computed min sizes: %dx%d\n", __func__, minWidth, minHeight);
    setMinimumSize(minWidth, minHeight);

}

MainWindow::~MainWindow()
{
    delete ui;
}
