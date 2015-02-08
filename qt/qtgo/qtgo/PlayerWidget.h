#ifndef PLAYERWIDGET_H
#define PLAYERWIDGET_H

#include <QWidget>

#include "Global.h"


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

    int playerType() const;
    int getAIStrength() const;
    void enableChoosingPlayer(bool enable);

private:

public slots:
    void setPlayerType(PlayerType type);
    void setPlayerTypeInt(int type);
    void showMenuExplicit();
    void showMenu(int playerTypeInt=0);
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

#endif // PLAYERWIDGET_H
