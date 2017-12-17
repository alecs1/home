#pragma once

#include "GoTable.h"

class GoTableWidget : public QWidget, public GoTable {
Q_OBJECT
public:
    explicit GoTableWidget(QWidget* parent = 0);
    ~GoTableWidget();

    void paintEvent(QPaintEvent *);
    void paint(QPaintDevice*) const;
    static float gridDist(float tableSize, int gameSize);
    static float stoneDiameter();
    void checkForResumeGame();

    bool loadGameAndStart(const QString fileName);
    bool loadGameFromRemote(const QJsonObject& json);

    void setSecondPlayerToNetwork();

    //TODO - temporary hack
    SGameSettings* getGameSettingsPointer();

public slots:
    void launchGamePressed(SGameSettings newSettings);
    void changeGameSettings(const SGameSettings &newSettings);
    void changeProgramSettings();
    bool playMove(const int row, const int col);
    bool passMove();
    bool undoMove();
    void finish(bool finishByResign);
    void activateEstimatingScore(bool estimate);
    void userConfirmedMove(int confirmed);
    void showPlayHints();
    void insertDefaultHandicap(int handicap);

private slots:
    bool AIPlayNextMove();
    void computeScoreAndUpdate();


signals:
    void gameStateChanged(GameState state);
    void movePlayed(int row, int col);
    void estimateScoreChanged(float score);
    void crtPlayerChanged(int player, PlayerType type, PlayerType oponentType);
    void askUserConfirmation(bool ask, int colour=EMPTY); //ask the user for confirmation, dialog belongs to another widget
    void pushGameSettings(SGameSettings newSettings);
    void highlightChanged(int col, int row);

protected:
    void resizeEvent(QResizeEvent *event);
    void mouseMoveEvent(QMouseEvent *ev);
    void mousePressEvent(QMouseEvent* ev);
    void mouseReleaseEvent(QMouseEvent* ev);

    //use QPoint as tuple of coordinates
    QPoint mouseToGameCoordinates(QMouseEvent* ev);

private:
    bool buildPixmaps(int diameter);
    void updateCursor();
    void updateSizes();
    void launchGame(bool resetTable = true);
    bool loadSaveGameFile(QString fileName);

private:
    QCursor* blackCursor = nullptr;
    QCursor* whiteCursor = nullptr;
    QCursor* redCursor = nullptr;
    QPixmap* blackStonePixmap = nullptr;
    QPixmap* whiteStonePixmap = nullptr;
    QPixmap* redStonePixmap = nullptr;
    float dist; //distance between two table lines
    float diameter;


    int highlightRow;
    int highlightCol;
    //position of the new stone between mouse press and mouse release
    int newStoneRow;
    int newStoneCol;

    //position of the new stone when game asks for confirmation
    int unconfirmedStoneRow;
    int unconfirmedStoneCol;


    bool showHints = false;

    bool askPlayConfirmation; //ask the user to confirm placement of a stone;
    bool acceptDoubleClickConfirmation = false;

    //populate this with some default settings, which are then passed to the game
    SGameSettings gameSettings;
    SProgramSettings* programSettings;

    bool cursorBlocked = false;

    //TODO: here or in the base?
    bool estimateScore = false;
    bool computing = false;
};
