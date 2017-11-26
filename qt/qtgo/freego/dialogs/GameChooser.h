#ifndef GAMECHOOSER_H
#define GAMECHOOSER_H

#include <QDialog>

namespace Ui {
class GameChooser;
}

class GameChooser : public QDialog
{
    Q_OBJECT

public:
    explicit GameChooser(QWidget *parent = 0);
    ~GameChooser();

private:
    Ui::GameChooser *ui;
};

#endif // GAMECHOOSER_H
