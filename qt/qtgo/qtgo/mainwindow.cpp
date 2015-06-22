#include <QFileDialog>
#include <QPropertyAnimation>

#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "DrawAreaWidget.h"
#include "GoTable.h"
#include "GameSettings.h"
#include "SaveFile.h"
#include "Settings.h"
#include "RoundInfo.h"
#include "MiniGameSettings.h"

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

    int narrowFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraCondensed-Regular.ttf");
    int wideFontId = fontDatabase.addApplicationFont(":/resources/fonts/StintUltraExpanded-Regular.ttf");
    printf("%s - font ids: %d, %d\n", __func__, narrowFontId, wideFontId);

    Settings::setMessageSender(this);
    SProgramSettings* programSettings = Settings::getProgramSettings();
    if (SaveFile::loadSettings(SaveFile::getDefSettingsFName(), programSettings) == false)
        Settings::populateDefaultProgramSettings(programSettings);

    //TODO - has to exist at the time GoTable is constructed, but it cannot be connected if settings emits a signal from inside the constructor
    gameSettingsWidget = new GameSettings(this);

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

    QObject::connect(this, SIGNAL(programSettingsChanged()), table, SLOT(changeProgramSettings()));
    QObject::connect(this, SIGNAL(programSettingsChanged()), drawArea, SLOT(changeProgramSettings()));
    QObject::connect(gameSettingsWidget, SIGNAL(gameSettingsChanged(SGameSettings)), table, SLOT(changeGameSettings(SGameSettings)));
    QObject::connect(gameSettingsWidget, SIGNAL(gameSettingsChanged(SGameSettings)), drawArea, SLOT(changeGameSettings(SGameSettings)));
    QObject::connect(gameSettingsWidget, SIGNAL(setMinimalInterface()), this, SLOT(setMinimalInterface()));
    QObject::connect(table, SIGNAL(gameStateChanged(GameState)), gameSettingsWidget, SLOT(setGameState(GameState)));
    QObject::connect(table, SIGNAL(estimateScoreChanged(float)), gameSettingsWidget, SLOT(setScoreEstimate(float)));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), gameSettingsWidget, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));

    QObject::connect(table, SIGNAL(askUserConfirmation(bool, int)), gameSettingsWidget, SLOT(showConfirmButton(bool, int)));
    QObject::connect(table, SIGNAL(pushGameSettings(SGameSettings)), gameSettingsWidget, SLOT(receiveSettings(SGameSettings)));
    QObject::connect(gameSettingsWidget, SIGNAL(launchGamePerform(SGameSettings)), table, SLOT(launchGamePressed(SGameSettings)));
    QObject::connect(gameSettingsWidget, SIGNAL(finishGamePerform(bool)), table, SLOT(finish(bool)));
    QObject::connect(gameSettingsWidget, SIGNAL(doEstimateScore(bool)), table, SLOT(activateEstimatingScore(bool)));
    QObject::connect(gameSettingsWidget, SIGNAL(userConfirmedMove(int)), table, SLOT(userConfirmedMove(int)));
    QObject::connect(gameSettingsWidget, SIGNAL(userPassedMove()), table, SLOT(passMove()));
    QObject::connect(gameSettingsWidget, SIGNAL(undoMove()), table, SLOT(undoMove()));
    QObject::connect(gameSettingsWidget, SIGNAL(showHints()), table, SLOT(showPlayHints()));
    QObject::connect(gameSettingsWidget, SIGNAL(saveGame()), this, SLOT(saveGame()));
    QObject::connect(gameSettingsWidget, SIGNAL(loadGame()), this, SLOT(loadGame()));


    table->checkForResumeGame();

    minWidth += gameSettingsWidget->sizeHint().width();
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
void MainWindow::setMinimalInterface() {
    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameSettingsWidget, "geometry");
    panelAnim->setDuration(1000);
    QRect original = gameSettingsWidget->geometry();
    QRect final = original;
    final.translate(-original.width(), -original.height());
    panelAnim->setStartValue(original);
    panelAnim->setEndValue(final);
    ui->gridLayout->removeWidget(gameSettingsWidget);
    panelAnim->start();


    if (miniGameSettings == NULL)
        createMiniInterface();

    miniGameSettings->show();
    miniGameSettings->resize(gameSettingsWidget->width()/2, gameSettingsWidget->height()/2);


    miniGameSettings->move(width(), height());
    original = miniGameSettings->geometry();
    final = original;
    final.translate(-original.width(), -height()/2);
    QPropertyAnimation *miniSettingsAnim = new QPropertyAnimation(miniGameSettings, "geometry");
    miniSettingsAnim->setDuration(1000);
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
    QPropertyAnimation *panelAnim = new QPropertyAnimation(gameSettingsWidget, "geometry");
    panelAnim->setDuration(1000);
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
    miniSettingsAnim->setDuration(1000);
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
     ui->gridLayout->addWidget(gameSettingsWidget, 0, 1);
     gameSettingsWidget->pushBackRoundInfo();
}

void MainWindow::createMiniInterface() {
    miniGameSettings = new MiniGameSettings(this);
    QObject::connect(miniGameSettings, SIGNAL(setFullInterface()), this, SLOT(setFullInterface()));
    QObject::connect(table, SIGNAL(crtPlayerChanged(int, PlayerType, PlayerType)), miniGameSettings, SLOT(setCurrentPlayer(int, PlayerType, PlayerType)));
}

