#pragma once

#include <QDialog>

#include "../GameStruct.h"

namespace Ui {
class GameInvitation;
}

class GameInvitation : public QDialog
{
Q_OBJECT

public:
    explicit GameInvitation(QWidget *parent = 0);
    ~GameInvitation();

    void setGridImageWidget(QWidget* widget);
    void setColour(const colors colour);

private:
    Ui::GameInvitation *ui;
};
