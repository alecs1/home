#ifndef HANDICAP_H
#define HANDICAP_H

#include <QWidget>

#include "Global.h"

class QMenu;

namespace Ui {
class Handicap;
}

class Handicap : public QWidget
{
    Q_OBJECT

public:
    explicit Handicap(SGameSettings::Handicap& handicap, QWidget *parent = 0);
    ~Handicap();

private slots:
    void komiActionChosen(QAction* action);
    void showKomiMenu();
    void setKomi(float value);
    void komiEditFinished();
    void handicapSelected(int value);
    void handicapPlacementSelected(int value);

public:
    void populateHandicap(SGameSettings::Handicap);

private:
    Ui::Handicap *ui;

    QMenu* komiMenu;
    SGameSettings::Handicap& handicap;
};

#endif // HANDICAP_H
