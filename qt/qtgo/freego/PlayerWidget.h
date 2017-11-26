#pragma once

#include <QWidget>

#include "Constants.h"
//#include "Global.h"


class QMenu;

namespace Ui {
class PlayerWidget;
}

class PlayerWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PlayerWidget(QWidget *parent = 0);
    ~PlayerWidget();
    void setPixmap(QPixmap aPixmap);


    void enableChoosingPlayer(bool enable);

//not all of these need to be slots, just keeping them together
public slots:
    PlayerType playerType() const;
    void setPlayerType(PlayerType type);
    void setPlayerTypeInt(int type);

    int getAIStrength() const;
    void setAIStrength(int strength);

    void showMenuExplicit();
    void showMenu(const PlayerType playerType = PlayerType::AI);
    void AIActionActivated(QAction* action);

signals:
    void playerTypeChanged(int);
    void playerStrengthChanged(int);

private:
    Ui::PlayerWidget *ui;
    QPixmap pixmap;
    QMenu* AIMenu;
    int AIStrength = 0;
};
