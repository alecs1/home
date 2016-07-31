#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QFileDialog>
#include <QPropertyAnimation>
//debug
#include <QToolButton>

#include "DrawAreaWidget.h"
#include "GoTable.h"
#include "GameSettings.h"
#include "SaveFile.h"
#include "Settings.h"
#include "RoundInfo.h"
#include "AboutDialog.h"
#include "HelpDialog.h"
#include "SettingsDialog.h"
#include "MiniGameSettings.h"
#include "ConfirmMoveDialog.h"
#include "DebugStuff.h"
#include "network/BTErrorDialog.h"
#include "network/PeerChooser.h"


#include "network/BTServer.h"
#include "network/ConnMan.h"

//TODO - just for tests
#include "btchat/chat.h"


#include "Global.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    printf("%s - %p\n", __func__, QThread::currentThreadId());

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

    int narrowFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraCondensed-Regular.ttf");
    int wideFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraExpanded-Regular.ttf");
    printf("%s - font ids: %d, %d\n", __func__, narrowFontId, wideFontId);

    Settings::setMessageSender(this);
    SProgramSettings* programSettings = Settings::getProgramSettings();
    if (SaveFile::loadSettings(SaveFile::getDefSettingsFName(), programSettings) == false)
        Settings::populateDefaultProgramSettings(programSettings);

    //TODO - has to exist at the time GoTable is constructed, but it cannot be connected if settings emits a signal from inside the constructor
    setupGameSettings();

    drawArea = new DrawAreaWidget(this);
    table = new GoTable(drawArea);
    drawArea->setChildTable(table);

    ui->centralWidget->setLayout(ui->gridLayout);
    ui->gridLayout->addWidget(drawArea, 0, 0);
    ui->gridLayout->setSpacing(0);
    ui->gridLayout->setColumnStretch(0, 5);
    ui->gridLayout->setColumnMinimumWidth(0, drawArea->minimumWidth());
    ui->gridLayout->setRowMinimumHeight(0, drawArea->minimumHeight());

    //TODO: this type of size computation is a poor hack, instead my table should provide a good minimum hint.
    int minWidth = drawArea->minimumSize().width();
    int minHeight = drawArea->minimumSize().height();


    ui->gridLayout->addWidget(gameSettingsWidget, 0, 1);

    connect(this, SIGNAL(programSettingsChanged()), table, SLOT(changeProgramSettings()));
    connect(this, SIGNAL(programSettingsChanged()), drawArea, SLOT(changeProgramSettings()));
    connect(gameSettingsWidget, SIGNAL(gameSettingsChanged(SGameSettings)), table, SLOT(changeGameSettings(SGameSettings)));
    connect(gameSettingsWidget, SIGNAL(gameSettingsChanged(SGameSettings)), drawArea, SLOT(changeGameSettings(SGameSettings)));
    connect(table, SIGNAL(gameStateChanged(GameState)), gameSettingsWidget, SLOT(setGameState(GameState)));
    connect(table, SIGNAL(gameStateChanged(GameState)), this, SLOT(setGameState(GameState)));
    connect(table, SIGNAL(estimateScoreChanged(float)), gameSettingsWidget, SLOT(setScoreEstimate(float)));
    connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), gameSettingsWidget, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));

    connect(table, SIGNAL(askUserConfirmation(bool, int)), gameSettingsWidget, SLOT(showConfirmButton(bool, int)));
    connect(table, SIGNAL(pushGameSettings(SGameSettings)), gameSettingsWidget, SLOT(receiveSettings(SGameSettings)));
    connect(gameSettingsWidget, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    connect(gameSettingsWidget, SIGNAL(finishGamePerform(bool)), table, SLOT(finish(bool)));
    connect(gameSettingsWidget, SIGNAL(doEstimateScore(bool)), table, SLOT(activateEstimatingScore(bool)));
    connect(gameSettingsWidget, SIGNAL(userConfirmedMove(int)), table, SLOT(userConfirmedMove(int)));
    connect(gameSettingsWidget, SIGNAL(userPassedMove()), table, SLOT(passMove()));
    connect(gameSettingsWidget, SIGNAL(undoMove()), table, SLOT(undoMove()));
    connect(gameSettingsWidget, SIGNAL(showHints()), table, SLOT(showPlayHints()));

    connect(ui->actionSave_Game, SIGNAL(triggered(bool)), this, SLOT(saveGame()));
    connect(ui->actionOpen_Saved_Game, SIGNAL(triggered(bool)), this, SLOT(loadGame()));
    connect(ui->actionPlay_on_Bluetooth, SIGNAL(triggered(bool)), this, SLOT(connectBT()));
    connect(ui->actionPlay_on_Network, SIGNAL(triggered(bool)), this, SLOT(connectTCP()));
    connect(ui->actionAdjust_for_Small_Display, SIGNAL(triggered(bool)), this, SLOT(setMinimalInterface()));
    connect(ui->actionSettings, SIGNAL(triggered(bool)), this, SLOT(showSettings()));
    connect(ui->actionHelp, SIGNAL(triggered(bool)), this, SLOT(showHelp()));
    connect(ui->actionAbout, SIGNAL(triggered(bool)), this, SLOT(showAbout()));
    connect(ui->actionDebug_BT, SIGNAL(triggered(bool)), this, SLOT(showBTChat()));


    table->checkForResumeGame();

    minWidth += gameSettingsWidget->sizeHint().width();
    minWidth *= 1.1;
    minHeight *= 1.27;


    printf("%s - computed min sizes: %dx%d\n", __func__, minWidth, minHeight);
    printf("%s - Widget style name=%s\n", __func__, QApplication::style()->objectName().toUtf8().constData());
    setMinimumSize(minWidth, minHeight);

    setWindowTitle("FreeGo");
    if (programSettings->minimalInterface)
        minimalInterface = true;

    //runDebug();
}

MainWindow::~MainWindow()
{
    printf("%s - TODO - fully implement this!\n", __func__);
    SaveFile::writeSettings(SaveFile::getDefSettingsFName(), Settings::getProgramSettings());
    delete ui;
}

void MainWindow::saveGame() {
    QString fileName = "";
    QFileDialog dialog(this, "Save game", "", "Json files(*.save)");
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
        if (!fileName.endsWith(".save"))
            fileName += ".save";
        result = table->saveGame(fileName);
    }
    printf("%s, fileName=%s, result=%d\n", __func__, fileName.toUtf8().constData(), result);
}

void MainWindow::loadGame() {
    QString fileName = "";
    QFileDialog dialog(this, "Open saved game", "", "Json files(*.save)");
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


void MainWindow::notifyReloadProgramSettings() {
    emit programSettingsChanged();
}

//TODO - does QPropertyAnimation cause a memory leak
//TODO - on Linux desktop you may not be allowed to move a window outside of the screen, so the game settings may stay visible.
void MainWindow::setMinimalInterface() {
    minimalInterface = true;
    Settings::getProgramSettings()->minimalInterface = true;
    SaveFile::writeSettings(SaveFile::getDefSettingsFName(), Settings::getProgramSettings());
    if (table->getGameState() != GameState::Started)
        return;

    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameSettingsWidget, "geometry");
    QRect original = gameSettingsWidget->geometry();
    QRect final = original;
    final.translate(-original.width(), -original.height());
    panelAnim->setStartValue(original);
    panelAnim->setEndValue(final);
    ui->gridLayout->removeWidget(gameSettingsWidget);
    panelAnim->start();


    if (miniGameSettings == nullptr) {
        createMiniInterface();
        confirmMoveDialog = new ConfirmMoveDialog(this);
        confirmMoveDialog->setMinimalInterface(true);
        confirmMoveDialog->setVisible(false);
        connect(table, SIGNAL(askUserConfirmation(bool, int)), this, SLOT(showConfirmDialog(bool, int)));
        connect(confirmMoveDialog, SIGNAL(finished(int)), this, SLOT(confirmDialogDone(int)));
        connect(confirmMoveDialog, SIGNAL(finished(int)), table, SLOT(userConfirmedMove(int)));
        connect(miniGameSettings, SIGNAL(userPassedMove()), table, SLOT(passMove()));
        connect(miniGameSettings, SIGNAL(undoMove()), table, SLOT(undoMove()));
    }

    miniGameSettings->show();
    miniGameSettings->resize(gameSettingsWidget->width()/2, gameSettingsWidget->height()/2);


    miniGameSettings->move(width(), height());
    original = miniGameSettings->geometry();
    final = original;
    final.translate(-original.width(), -height()/2);
    QPropertyAnimation *miniSettingsAnim = new QPropertyAnimation(miniGameSettings, "geometry");
    miniSettingsAnim->setStartValue(original);
    miniSettingsAnim->setEndValue(final);
    miniSettingsAnim->start();
    QObject::connect(miniSettingsAnim, SIGNAL(finished()), this, SLOT(transitionToMinDone()));

    if (roundInfo == NULL)
        roundInfo = gameSettingsWidget->popRoundInfo();
    roundInfo->setParent(this);
    roundInfo->show();
    roundInfo->move(width() - gameSettingsWidget->width(), 0);
    roundInfo->setLayoutDirection(false);
    original = roundInfo->geometry();
    final = original;
    final.moveTo(width() - roundInfo->width(), 0);
    QPropertyAnimation *roundInfoAnim = new QPropertyAnimation(roundInfo, "geometry");
    roundInfoAnim->setStartValue(original);
    roundInfoAnim->setEndValue(final);
    roundInfoAnim->start();
}

void MainWindow::transitionToMinDone() {
    printf("%s\n", __func__);
    printf("%s - roundInfo.visible=%d\n", __func__, roundInfo->isVisible());
    ui->gridLayout->addWidget(miniGameSettings, 0, 1);
    miniGameSettings->addRoundInfo(roundInfo);
}

void MainWindow::setFullInterface() {
    minimalInterface = false;
    restoreFullInterface();
}

void MainWindow::restoreFullInterface() {
    if (ui->gridLayout->itemAtPosition(0, 1)->widget() == gameSettingsWidget) {
        return;
    }
    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameSettingsWidget, "geometry");
    QRect original = gameSettingsWidget->geometry();
    QRect final = original;
    final.translate(original.width(), original.height());
    panelAnim->setStartValue(original);
    panelAnim->setEndValue(final);
    panelAnim->start();
    QObject::connect(panelAnim, SIGNAL(finished()), this, SLOT(transitionToFullDone()));

    ui->gridLayout->removeWidget(miniGameSettings);
    original = miniGameSettings->geometry();
    final = original;
    final.translate(width() + original.width(), height() + original.height());
    printf("%s - original: (%d,%d), final: (%d, %d), size: (%d, %d)\n", __func__,
           original.x(), original.y(), final.x(), final.y(), final.width(), final.height());
    QPropertyAnimation *miniSettingsAnim = new QPropertyAnimation(miniGameSettings, "geometry");
    miniSettingsAnim->setStartValue(original);
    miniSettingsAnim->setEndValue(final);
    miniSettingsAnim->start();

    if (roundInfo == NULL)
        roundInfo = gameSettingsWidget->popRoundInfo();
    roundInfo->setParent(this);
    roundInfo->show();
    roundInfo->setLayoutDirection(true);
    roundInfo->move(width() - miniGameSettings->width(), 0);
    original = roundInfo->geometry();
    final = original;
    final.moveTo(width() - miniGameSettings->width()*2.5, 0); //maybe the full geometry has never been shown
    QPropertyAnimation *roundInfoAnim = new QPropertyAnimation(roundInfo, "geometry");
    roundInfoAnim->setStartValue(original);
    roundInfoAnim->setEndValue(final);
    roundInfoAnim->start();
}

void MainWindow::transitionToFullDone() {
    gameSettingsWidget->pushBackRoundInfo();
    ui->gridLayout->addWidget(gameSettingsWidget, 0, 1);
}

/**
 * @brief MainWindow::setupGameSettings
 * Initialise the GameSettings widget
 */
void MainWindow::setupGameSettings() {
    gameSettingsWidget = new GameSettings(this);
    QList<QAction*> actions;
    actions.append(ui->actionSave_Game);
    actions.append(ui->actionOpen_Saved_Game);
    actions.append(ui->actionSettings);
    actions.append(ui->actionPlay_on_Bluetooth);
    actions.append(ui->actionPlay_on_Network);
    actions.append(ui->actionHelp);
    actions.append(ui->actionAbout);
    actions.append(ui->actionAdjust_for_Small_Display);
    actions.append(ui->actionDebug_BT);
    gameSettingsWidget->setActions(actions);
}

void MainWindow::createMiniInterface() {
    miniGameSettings = new MiniGameSettings(this);
    QObject::connect(miniGameSettings, SIGNAL(setFullInterface()), this, SLOT(setFullInterface()));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), miniGameSettings, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));
}


void MainWindow::showConfirmDialog(bool show, int colour) {
    if (!minimalInterface)
        return;
    if (show) {
        ui->gridLayout->removeWidget(miniGameSettings);
        miniGameSettings->hide();
        ui->gridLayout->addWidget(confirmMoveDialog, 0, 1);
        confirmMoveDialog->show();
    }
    else {
        ui->gridLayout->removeWidget(confirmMoveDialog);
        confirmMoveDialog->hide();
        ui->gridLayout->addWidget(miniGameSettings, 0, 1);
        miniGameSettings->show();
    }
    Q_UNUSED(colour);
}

void MainWindow::confirmDialogDone(int confirmed) {
    showConfirmDialog(false, -1);
    printf("%s, confirmed = %d\n", __func__, confirmed);
}

void MainWindow::setGameState(GameState state) {
    if (minimalInterface) {
        if (state == GameState::Started)
            setMinimalInterface();
        else
            restoreFullInterface();
    }
}

int MainWindow::connectBT() {
#ifndef _WIN32
    printf("%s - %p\n", __func__, QThread::currentThreadId());
    int ret = -1;
    if (btServer == nullptr) {
        if (connMan == nullptr) {
            connMan = new ConnMan;
        }
        btServer = new BTServer(connMan);
        ret = btServer->initBluetooth(0);
        if (ret == -1) {
            delete btServer;
            btServer = nullptr;

            BTErrorDialog dialog("Error initialising bluetooth:\n" + QString::number(ret));
            int result = dialog.exec();
            return result;
        }
    }
    else {
        printf("%s - btServer was already initialised\n", __func__);
    }

    printf("Showing peerChooser\n");

    //TODO - memleak, fix this.
    PeerChooser* p = new PeerChooser(*btServer, nullptr);
    p->show();

    printf("%s - ran, ret=%d\n", __func__, ret);
#else
    printf("No QBluetooth support on Windows\n");
#endif
    return 0;
}

void MainWindow::connectTCP() {
    if (connMan == NULL) {
        connMan = new ConnMan;
    }
    connMan->connectTCP();
}

void MainWindow::showBTChat() {
#ifndef _WIN32
    Chat* btChat = new Chat(this);
    ui->gridLayout->addWidget(btChat, 0, 0);
#else
    printf("No QBluetooth support on Windows\n");
#endif
}

void MainWindow::showAbout() {
    AboutDialog dialog;
    dialog.exec();
    printf("%s - done\n", __func__);
}

void MainWindow::showHelp() {
    HelpDialog dialog;
    dialog.exec();
}

void MainWindow::showSettings() {
    SettingsDialog dialog;
    dialog.exec();
}

