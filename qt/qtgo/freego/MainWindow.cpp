#include "MainWindow.h"
#include "ui_MainWindow.h"

#include <QFileDialog>
#include <QPropertyAnimation>
#include <QMessageBox>
#include <QToolButton>
#include <QJsonDocument>

#include "DrawAreaWidget.h"
#include "GoTable.h"
#include "GameControlWidget.h"
#include "SaveFile.h"
#include "Settings.h"
#include "RoundInfo.h"
#include "AboutDialog.h"
#include "HelpDialog.h"
#include "SettingsDialog.h"
#include "MiniGameControlWidget.h"
#include "ConfirmMoveDialog.h"
#include "DebugStuff.h"
#include "Logger.h"

#include "network/BTServer.h"
#include "network/ConnMan.h"
#include "network/PeerChooser.h"

#include "notifications/DockedNotif.h"

#include "network/ProtoJson.h"

#include "dialogs/AddressDialog.h"

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

    printf("%s - setting fullscreen for Android - TODO: does not work!!!\n", __func__);
    showFullScreen();
    printf("%s - is fullscreen=%d\n", __func__, isFullScreen());
    #endif

    int narrowFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraCondensed-Regular.ttf");
    int wideFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraExpanded-Regular.ttf");
    printf("%s - font ids: %d, %d\n", __func__, narrowFontId, wideFontId);

    Settings::setMessageSender(this);
    SProgramSettings* programSettings = Settings::getProgramSettings();
    if (SaveFile::loadSettings(SaveFile::getDefSettingsFName(), programSettings) == false) {
        Settings::populateDefaultProgramSettings(programSettings);
    }

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


    ui->gridLayout->addWidget(gameControlWidget, 0, 1);

    connect(this, SIGNAL(programSettingsChanged()), table, SLOT(changeProgramSettings()));
    connect(this, SIGNAL(programSettingsChanged()), drawArea, SLOT(changeProgramSettings()));
    connect(gameControlWidget, SIGNAL(gameSettingsChanged(SGameSettings)), table, SLOT(changeGameSettings(SGameSettings)));
    connect(gameControlWidget, SIGNAL(gameSettingsChanged(SGameSettings)), drawArea, SLOT(changeGameSettings(SGameSettings)));

    connect(table, SIGNAL(gameStateChanged(GameState)), gameControlWidget, SLOT(setGameState(GameState)));
    connect(table, SIGNAL(gameStateChanged(GameState)), this, SLOT(setGameState(GameState)));
    connect(table, SIGNAL(movePlayed(int, int)), this, SLOT(onMovePlayed(int,int)));
    connect(table, SIGNAL(estimateScoreChanged(float)), gameControlWidget, SLOT(setScoreEstimate(float)));
    connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), gameControlWidget, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));
    connect(table, SIGNAL(askUserConfirmation(bool, int)), this, SLOT(showConfirmDialog(bool, int)));
    connect(table, SIGNAL(pushGameSettings(SGameSettings)), gameControlWidget, SLOT(receiveSettings(SGameSettings)));


    connect(gameControlWidget, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    connect(gameControlWidget, SIGNAL(resign()), this, SLOT(onResign()));
    connect(gameControlWidget, SIGNAL(doEstimateScore(bool)), table, SLOT(activateEstimatingScore(bool)));
    connect(gameControlWidget, SIGNAL(userConfirmedMove(int)), table, SLOT(userConfirmedMove(int)));
    connect(gameControlWidget, SIGNAL(userPassedMove()), table, SLOT(passMove()));
    connect(gameControlWidget, SIGNAL(undoMove()), table, SLOT(undoMove()));
    connect(gameControlWidget, SIGNAL(showHints()), table, SLOT(showPlayHints()));

    connect(ui->actionSave_Game, SIGNAL(triggered(bool)), this, SLOT(saveGame()));
    connect(ui->actionOpen_Saved_Game, SIGNAL(triggered(bool)), this, SLOT(loadGame()));
    connect(ui->actionPlay_on_Bluetooth, SIGNAL(triggered(bool)), this, SLOT(connectBT()));
    connect(ui->actionPlay_on_Network, SIGNAL(triggered(bool)), this, SLOT(connectTCP()));
    connect(ui->actionAdjust_for_Small_Display, SIGNAL(triggered(bool)), this, SLOT(setMinimalInterface()));
    connect(ui->actionSettings, SIGNAL(triggered(bool)), this, SLOT(showSettings()));
    connect(ui->actionHelp, SIGNAL(triggered(bool)), this, SLOT(showHelp()));
    connect(ui->actionAbout, SIGNAL(triggered(bool)), this, SLOT(showAbout()));

    connMan = new ConnMan(this);
    connect(connMan, SIGNAL(connStateChanged(ConnMan::ConnState, bool, ConnMan::ConnType)), this, SLOT(onConnStateChanged(ConnMan::ConnState, bool, ConnMan::ConnType)));
    connMan->listenTCP();

    table->checkForResumeGame();

    minWidth += gameControlWidget->sizeHint().width();
    minWidth *= 1.1;
    minHeight *= 1.27;


    printf("%s - computed min sizes: %dx%d\n", __func__, minWidth, minHeight);
    printf("%s - Widget style name=%s\n", __func__, QApplication::style()->objectName().toUtf8().constData());
    setMinimumSize(minWidth, minHeight);

    setWindowTitle("FreeGo");
    if (programSettings->minimalInterface) {
        setMinimalInterface();
    }

    Logger::setViewer(ui->logView);
    if (platformType() == PlatformType::Android) {
        ui->logView->hide();
    }

    double mainLoopInterval = 1000.0 / 60;
    Logger::log(QString("mainLoopInterval: %1").arg(mainLoopInterval));
    printf("mainLoopInterval: %g\n", mainLoopInterval);
    mainLoopTimer.setInterval(mainLoopInterval);
    mainLoopTimer.start();
    connect(&mainLoopTimer, SIGNAL(timeout()), this, SLOT(mainLoop()));
}

MainWindow::~MainWindow() {
    Logger::setViewer(nullptr); //TODO - this is such a crappy solution to avoid crashes, use smart pointers
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
        result = table->loadGameAndStart(fileName);
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

    if (miniGameControlWidget == nullptr) {
        createMiniInterface();
        connect(miniGameControlWidget, SIGNAL(userPassedMove()), table, SLOT(passMove()));
        connect(miniGameControlWidget, SIGNAL(undoMove()), table, SLOT(undoMove()));
    }

    if (table->getGameState() != GameState::Started)
        return;

    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameControlWidget, "geometry");
    QRect original = gameControlWidget->geometry();
    QRect final = original;
    final.translate(-original.width(), -original.height());
    panelAnim->setStartValue(original);
    panelAnim->setEndValue(final);
    ui->gridLayout->removeWidget(gameControlWidget);
    panelAnim->start();


    miniGameControlWidget->show();
    miniGameControlWidget->resize(gameControlWidget->width()/2, gameControlWidget->height()/2);
    miniGameControlWidget->move(width(), height());
    original = miniGameControlWidget->geometry();
    final = original;
    final.translate(-original.width(), -height()/2);
    QPropertyAnimation *miniSettingsAnim = new QPropertyAnimation(miniGameControlWidget, "geometry");
    miniSettingsAnim->setStartValue(original);
    miniSettingsAnim->setEndValue(final);
    miniSettingsAnim->start();
    QObject::connect(miniSettingsAnim, SIGNAL(finished()), this, SLOT(transitionToMinDone()));

    if (roundInfo == NULL)
        roundInfo = gameControlWidget->popRoundInfo();
    roundInfo->setParent(this);
    roundInfo->show();
    roundInfo->move(width() - gameControlWidget->width(), 0);
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
    ui->gridLayout->addWidget(miniGameControlWidget, 0, 1);
    miniGameControlWidget->addRoundInfo(roundInfo);
}

void MainWindow::setFullInterface() {
    minimalInterface = false;
    restoreFullInterface();
}

void MainWindow::restoreFullInterface() {
    if (ui->gridLayout->itemAtPosition(0, 1)->widget() == gameControlWidget) {
        return;
    }
    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameControlWidget, "geometry");
    QRect original = gameControlWidget->geometry();
    QRect final = original;
    final.translate(original.width(), original.height());
    panelAnim->setStartValue(original);
    panelAnim->setEndValue(final);
    panelAnim->start();
    QObject::connect(panelAnim, SIGNAL(finished()), this, SLOT(transitionToFullDone()));

    ui->gridLayout->removeWidget(miniGameControlWidget);
    original = miniGameControlWidget->geometry();
    final = original;
    final.translate(width() + original.width(), height() + original.height());
    printf("%s - original: (%d,%d), final: (%d, %d), size: (%d, %d)\n", __func__,
           original.x(), original.y(), final.x(), final.y(), final.width(), final.height());
    QPropertyAnimation *miniSettingsAnim = new QPropertyAnimation(miniGameControlWidget, "geometry");
    miniSettingsAnim->setStartValue(original);
    miniSettingsAnim->setEndValue(final);
    miniSettingsAnim->start();

    if (roundInfo == nullptr) {
        roundInfo = gameControlWidget->popRoundInfo();
    }
    roundInfo->setParent(this);
    roundInfo->show();
    roundInfo->setLayoutDirection(true);
    roundInfo->move(width() - miniGameControlWidget->width(), 0);
    original = roundInfo->geometry();
    final = original;
    final.moveTo(width() - miniGameControlWidget->width()*2.5, 0); //maybe the full geometry has never been shown
    QPropertyAnimation *roundInfoAnim = new QPropertyAnimation(roundInfo, "geometry");
    roundInfoAnim->setStartValue(original);
    roundInfoAnim->setEndValue(final);
    roundInfoAnim->start();
}

void MainWindow::transitionToFullDone() {
    gameControlWidget->pushBackRoundInfo();
    ui->gridLayout->addWidget(gameControlWidget, 0, 1);
}

/**
 * @brief MainWindow::setupGameSettings
 * Initialise the GameSettings widget
 */
void MainWindow::setupGameSettings() {
    gameControlWidget = new GameControlWidget(this);
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
    //for android we disable network and stuff for now
#if defined (Q_OS_ANDROID)
    if (false) {
        ui->actionPlay_on_Bluetooth->setEnabled(false);
        ui->actionPlay_on_Network->setEnabled(false);
        ui->actionDebug_BT->setEnabled(false);
    }
#endif

    gameControlWidget->setActions(actions);
}

void MainWindow::createMiniInterface() {
    miniGameControlWidget = new MiniGameControlWidget(this);
    QObject::connect(miniGameControlWidget, SIGNAL(setFullInterface()), this, SLOT(setFullInterface()));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), miniGameControlWidget, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));
    miniGameControlWidget->hide();
}


void MainWindow::showConfirmDialog(bool show, int colour) {
    if (!confirmMoveDialog) {
        confirmMoveDialog = new ConfirmMoveDialog(this);
        confirmMoveDialog->setVisible(false);
        connect(confirmMoveDialog, SIGNAL(finished(int)), this, SLOT(confirmDialogDone(int)));
        connect(confirmMoveDialog, SIGNAL(finished(int)), table, SLOT(userConfirmedMove(int)));
    }
    confirmMoveDialog->setMinimalInterface(minimalInterface);

    if (show) {
        if (minimalInterface) {
            ui->gridLayout->removeWidget(miniGameControlWidget);
            miniGameControlWidget->hide();
        }
        else {
            ui->gridLayout->removeWidget(this->gameControlWidget);
            gameControlWidget->hide();
        }
        ui->gridLayout->addWidget(confirmMoveDialog, 0, 1);
        confirmMoveDialog->show();
    }
    else {
        ui->gridLayout->removeWidget(confirmMoveDialog);
        confirmMoveDialog->hide();
        if (minimalInterface) {
            ui->gridLayout->addWidget(miniGameControlWidget, 0, 1);
            miniGameControlWidget->show();
        }
        else {
            ui->gridLayout->addWidget(gameControlWidget, 0, 1);
            gameControlWidget->show();
        }
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

//TODO - disable playing when the network peer has disconnected
void MainWindow::onMovePlayed(int row, int col) {
    Logger::log(QString("%1: %2 %3").arg(__func__).arg(row).arg(col));
    int crtPlayer;
    PlayerType crtType, oppType;
    table->getPlayersState(crtPlayer, crtType, oppType);
    Logger::log(QString("crtPlayer: %1 crtType: %2 oppType: %3").arg(crtPlayer).arg(playerTypeMap.left.at(crtType)).arg(playerTypeMap.left.at(oppType)));
    if (crtType == PlayerType::Network) {
        ProtoJson::Msg msg;
        msg.type = ProtoJson::MsgType::PlayMove;
        msg.json["row"] = row;
        msg.json["col"] = col;
        connMan->sendMessage(msg);
        activeMessage = msg;
    }
}

void MainWindow::onResign() {
    bool success = table->playMove(FREEGO_RESIGN_MOVE, FREEGO_RESIGN_MOVE);
}

int MainWindow::connectBT() {
#ifndef _WIN32
    printf("%s - %p\n", __func__, QThread::currentThreadId());

    if (peerChooser == nullptr) {
        peerChooser = new PeerChooser(*connMan, this);
    }
    peerChooser->show();

#else
    Logger::log("No QBluetooth support on Windows", Logger::ERR);
#endif
    return 0;
}

void MainWindow::connectTCP() {
    AddressDialog addressDialog(Settings::getProgramSettings()->previousTCPAddresses, this);
    int result = addressDialog.exec();
    QString address;
    int port = 0;
    if (result == QDialog::Accepted) {
        address = addressDialog.address();
        if (address.contains(":")) {
            QStringList auxAddr = address.split(":");
            if (auxAddr.length() > 0)
                address = auxAddr[0];
            if (auxAddr.length() > 1)
                port = auxAddr[1].toInt();
            if (auxAddr.length() > 2) {
                Logger::log(QString("Address may be invalid: %1").arg(addressDialog.address()), Logger::ERR);
            }
        }
    }
    else {
        return;
    }

    //In case of parsing problems address and port get the default values.
    bool success = connMan->connectTCP(address, port);
    if (success && address.length() > 0) {
        QStringList previousAddr = Settings::getProgramSettings()->previousTCPAddresses;
        if (!previousAddr.contains(address)) {
            Settings::getProgramSettings()->previousTCPAddresses.insert(0, address);
        }
    }
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

void MainWindow::mainLoop() {
    if (connMan) {
        connMan->update();
    }
}

void MainWindow::onConnStateChanged(ConnMan::ConnState state, bool initiator, ConnMan::ConnType connType) {
    Logger::log(QString("%1 - %2 %3 %4").arg(__PRETTY_FUNCTION__).arg(state).arg(initiator).arg(connType));
    switch (state) {
    case ConnMan::ConnState::Connected: {
        if (initiator) {
            //hack to get this running: propose a game with the current state of our board
            ProtoJson::Msg msg;
            msg.type = ProtoJson::ResumeGame;
            QJsonObject tableSetupJson;
            table->saveGameForRemote(tableSetupJson);
            msg.json["gameSetup"] = tableSetupJson;
            connMan->sendMessage(msg);
            activeMessage = msg;
            Logger::log(QString("Will initiate a new game with: %1.").arg(QJsonDocument(msg.json).toJson().constData()));
//            if (!makeSettingsDock) {
//                QList<notifications::Option> options;
//                options << notifications::OPTION_DONE << notifications::OPTION_CANCEL;
//                makeSettingsDock = new notifications::DockedNotif("Setup the game to play with the remove player and press \"Done\"", options);
//            }
//            addDockWidget(Qt::TopDockWidgetArea, makeSettingsDock);
        }
        else {

        }
        break;
    }
    default:
        break;
    }
}

void MainWindow::onRemoteMessage(const ProtoJson::Msg& msg) {
    ProtoJson::Msg reply;
    reply.msgid = msg.msgid;

    if (msg.type >= ProtoJson::MsgType::Success && msg.type <= ProtoJson::MsgType::Error) {
        if (msg.msgid != activeMessage.msgid) {
            Logger::log(QString("Received unexpected reply to msg %1, expected %2").arg(msg.msgid).arg(activeMessage.msgid));
            return;
        }

        if (msg.type == ProtoJson::MsgType::Success) {
            if (activeMessage.type == ProtoJson::MsgType::ResumeGame) {
                //Yay, our peer accepted our game!
                Logger::log("Peer accepted our game!");
                table->setSecondPlayerToNetwork();
            }
            else if (activeMessage.type == ProtoJson::MsgType::PlayMove) {
                Logger::log("Peer accepted our move");
            }
        }
    }
    else if (msg.type == ProtoJson::MsgType::ResumeGame) {
        QJsonObject json = msg.json[ProtoJson::ProtoKw::Request].toObject();
        QJsonDocument doc(json);

       //TODO - confirmation must show how the set-up game will look like
        int ret = QMessageBox::question(this, "Remote game", QString("Remote player wants to start a new game. Accepting will delete your current game. Accept? \n%1").arg(doc.toJson().constData()));
        if (ret == QMessageBox::Yes) {
            Logger::log(QString("Accepted game: %1").arg(doc.toJson().constData()), Logger::DBG);
            bool success = table->loadGameFromRemote(json["gameSetup"].toObject());
            reply.type = success ? ProtoJson::MsgType::Success : ProtoJson::MsgType::Error;
        }
        else {
            //nothing, should refuse
            QJsonObject json = msg.json;
            QJsonDocument doc(json);
            Logger::log(QString("refused game: %1").arg(doc.toJson().constData()), Logger::DBG);
            reply.type = ProtoJson::MsgType::Fail;
        }
        connMan->sendMessage(reply);
    }

    else if (msg.type == ProtoJson::MsgType::PlayMove) {
        int crtPlayer;
        PlayerType crtType, oppType;
        table->getPlayersState(crtPlayer, crtType, oppType);

        if (crtType == PlayerType::Network) {
            QJsonObject json = msg.json[ProtoJson::ProtoKw::Request].toObject();
            int row = json["row"].toInt();
            int col = json["col"].toInt();
            bool success = table->playMove(row, col);
            reply.type = success ? ProtoJson::MsgType::Success : ProtoJson::MsgType::Fail;
        }
        else {
            reply.type = ProtoJson::MsgType::Error;
        }
        connMan->sendMessage(reply);
    }
}

