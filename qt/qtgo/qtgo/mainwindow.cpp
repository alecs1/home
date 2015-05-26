#include <QFileDialog>

#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "GoTable.h"
#include "GameSettings.h"

#include "Global.h"

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

    //TODO - has to exist at the time GoTable is constructed, but it cannot be connected if if settings emits a signal from inside the constructor
    GameSettings* settings = new GameSettings(this);

    table = new GoTable(this);
    ui->centralWidget->setLayout(ui->gridLayout);

    ui->gridLayout->addWidget(table, 0, 0);
    ui->gridLayout->setColumnStretch(0, 5);
    ui->gridLayout->setColumnMinimumWidth(0, table->minimumWidth());
    ui->gridLayout->setRowMinimumHeight(0, table->minimumHeight());

    //TODO: this type of size computation is a poor hack, instead my table should provide a good minimum hint.
    int minWidth = table->minimumSize().width();
    int minHeight = table->minimumSize().height();


    ui->gridLayout->addWidget(settings, 0, 1);
    ui->gridLayout->setColumnStretch(1, 1);
    QObject::connect(table, SIGNAL(gameStateChanged(GameState)), settings, SLOT(setGameState(GameState)));
    QObject::connect(table, SIGNAL(estimateScoreChanged(float)), settings, SLOT(setScoreEstimate(float)));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), settings, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));
    QObject::connect(table, SIGNAL(askUserConfirmation(bool, int)), settings, SLOT(showConfirmButton(bool, int)));
    QObject::connect(table, SIGNAL(pushGameSettings(SGameSettings)), settings, SLOT(receiveSettings(SGameSettings)));
    QObject::connect(settings, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    QObject::connect(settings, SIGNAL(finishGamePerform(bool)), table, SLOT(finish(bool)));
    QObject::connect(settings, SIGNAL(doEstimateScore(bool)), table, SLOT(activateEstimatingScore(bool)));
    QObject::connect(settings, SIGNAL(userConfirmedMove(int)), table, SLOT(userConfirmedMove(int)));
    QObject::connect(settings, SIGNAL(userPassedMove()), table, SLOT(passMove()));
    QObject::connect(settings, SIGNAL(undoMove()), table, SLOT(undoMove()));
    QObject::connect(settings, SIGNAL(showHints()), table, SLOT(showPlayHints()));
    QObject::connect(settings, SIGNAL(gameSettingsChanged(SGameSettings)), table, SLOT(changeGameSettings(SGameSettings)));
    QObject::connect(settings, SIGNAL(saveGame()), this, SLOT(saveGame()));
    QObject::connect(settings, SIGNAL(loadGame()), this, SLOT(loadGame()));


    table->checkForResumeGame();

    minWidth += settings->sizeHint().width();
    minWidth *= 1.1;
    minHeight *= 1.27;


    printf("%s - computed min sizes: %dx%d\n", __func__, minWidth, minHeight);
    printf("%s - Widget style name=%s\n", __func__, QApplication::style()->objectName().toUtf8().constData());
    setMinimumSize(minWidth, minHeight);

    setWindowTitle("FreeGo");

}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::saveGame() {
    //QString fileName = QFileDialog::getSaveFileName(this, "Save game", "", "Json files(*.json)");
    QString fileName = "";
    QFileDialog dialog(this, "Save game", "", "Json files(*.json)");
    if (platformType() == PlatformType::Android) {
        //dialog.setWindowState(Qt::WindowFullScreen); //does not work correctly on Android
        dialog.resize(this->size());
        dialog.setOption(QFileDialog::HideNameFilterDetails);
        dialog.setViewMode(QFileDialog::List);
    }
    dialog.setFileMode(QFileDialog::AnyFile);
    if (dialog.exec()) {
        fileName = dialog.selectedFiles()[0];
    }
    bool result = false;
    if (fileName != "") {
        if (!fileName.endsWith(".json"))
            fileName += ".json";
        result = table->saveGame(fileName);
    }
    printf("%s, fileName=%s, result=%d\n", __func__, fileName.toUtf8().constData(), result);
}

void MainWindow::loadGame() {
    QString fileName = "";
    QFileDialog dialog(this, "Open saved game", "", "Json files(*.json)");
    if (platformType() == PlatformType::Android) {
        //dialog.setWindowState(Qt::WindowFullScreen);
        dialog.resize(this->size());
        dialog.setOption(QFileDialog::HideNameFilterDetails);
        dialog.setViewMode(QFileDialog::List);
    }
    dialog.setFileMode(QFileDialog::ExistingFile);
    if (dialog.exec()) {
        fileName = dialog.selectedFiles()[0];
    }

    bool result = false;
    if (fileName != "") {
        result = table->loadGame(fileName);
    }
    //TODO - note an error somewhere
    printf("%s, fileName=%s, result=%d\n", __func__, fileName.toUtf8().constData(), result);
}
