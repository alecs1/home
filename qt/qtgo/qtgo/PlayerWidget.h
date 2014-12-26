#ifndef PLAYERWIDGET_H
#define PLAYERWIDGET_H

#include <QWidget>

#include "Global.h"

namespace Ui {
class PlayerWidget;
}

class PlayerWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PlayerWidget(QWidget *parent = 0);
    ~PlayerWidget();
    void setTitle(QString title);

    int playerType() const;
    void setPlayerType(PlayerType type);

private:
    Ui::PlayerWidget *ui;
};

#endif // PLAYERWIDGET_H
