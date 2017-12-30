#pragma once

#include <QDialog>

namespace Ui {
class GameInvitation;
}

class GameInvitation : public QDialog
{
Q_OBJECT

public:
    explicit GameInvitation(QWidget *parent = 0);
    ~GameInvitation();

    void setImageWidget(QWidget* widget);

private:
    Ui::GameInvitation *ui;
};
