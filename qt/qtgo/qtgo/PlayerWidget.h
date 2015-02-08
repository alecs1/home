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
    void setPixmap(QPixmap aPixmap);

    int playerType() const;
    void setPlayerType(PlayerType type);
    void enableChoosingPlayer(bool enable);

signals:
    void playerTypeChanged(int);

private:
    Ui::PlayerWidget *ui;
    QPixmap pixmap;
};

#endif // PLAYERWIDGET_H
