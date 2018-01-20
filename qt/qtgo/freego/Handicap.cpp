#include <QMenu>

#include <cmath>

#include "Handicap.h"
#include "ui_Handicap.h"

const QList<float> defaultKomiValues { 0, 0.5, 4.5, 5.5, 6.5, 7, 7.5, 8.5 };
const int defKomiIndex = 5;
const float maxKomi = 65;
const float minKomi = -10;
const int maxHandicap = 9;

Handicap::Handicap(SGameSettings::Handicap &handicap, QWidget *parent) :
    handicap(handicap),
    QWidget(parent),
    ui(new Ui::Handicap)
{
    ui->setupUi(this);
    ui->komiEdit->hide();

    komiMenu = new QMenu(ui->komiButton);
    for (int i = 0; i < defaultKomiValues.size(); i++) {
        QString text;
        text.setNum(defaultKomiValues[i], 'g', 2);
        komiMenu->addAction(text);
    }

    ui->handicapCombo->insertItem(0, "No handicap");
    for(int i = 2; i <= maxHandicap; i++) {
        QString text = QString::number(i) + " stones";
        ui->handicapCombo->insertItem(i, text);
    }

    ui->handicapPlacementCombo->insertItem(0, "fixed");
    ui->handicapPlacementCombo->insertItem(1, "free");
    ui->handicapPlacementCombo->setItemData(1, false, Qt::UserRole - 1);

    populateHandicap(handicap);

    connect(ui->komiButton, SIGNAL(clicked()), this, SLOT(showKomiMenu()));
    connect(komiMenu, SIGNAL(triggered(QAction*)), this, SLOT(komiActionChosen(QAction*)));
    connect(ui->komiInputButton, SIGNAL(clicked()), ui->komiEdit, SLOT(show()));
    connect(ui->komiEdit, SIGNAL(editingFinished()), this, SLOT(komiEditFinished()));
    connect(ui->handicapCombo, SIGNAL(activated(int)), this, SLOT(handicapSelected(int)));
    connect(ui->handicapPlacementCombo, SIGNAL(activated(int)), this, SLOT(handicapPlacementSelected(int)));
}

Handicap::~Handicap()
{
    delete ui;
}

void Handicap::komiActionChosen(QAction* action) {
    for(int i = 0; i < komiMenu->actions().size(); i++) {
        if (action == komiMenu->actions()[i]) {
            komiMenu->setDefaultAction(action);
            float value = defaultKomiValues[i];
            printf("%s - chose komi:%g\n", __func__, value);
            setKomi(value);
        }
    }
}

void Handicap::showKomiMenu() {
    komiMenu->show();
    QPoint komiButtonCenter = ui->komiButton->pos();
    komiButtonCenter.setX(komiButtonCenter.x() + ui->komiButton->width()/2 - komiMenu->width()/2);
    komiButtonCenter.setY(komiButtonCenter.y() + ui->komiButton->height()/2 - komiMenu->height()/2);
    QPoint globalPos = mapToGlobal(komiButtonCenter);
    //globalPos.setX(globalPos.x() - komiMenu->size().width());
    komiMenu->move(globalPos);
}

void Handicap::setKomi(float value) {
    handicap.komi = value;
    QString komiVal = QString::number(value, 'g', 2);
    ui->komiButton->setText("Komi: " + internal_state->komiVal);
    ui->komiEdit->setText(komiVal);
    for(int i = 0; i < defaultKomiValues.size(); i++) {
        if (fabs(defaultKomiValues[i] - value) < 0.1) {
            komiMenu->setDefaultAction(komiMenu->actions()[i]);
        }
    }
}

void Handicap::komiEditFinished() {
    QString strVal = ui->komiEdit->text();
    bool parsed;
    float val = strVal.toFloat(&parsed);
    if (parsed && (val >= minKomi) && (val <= maxKomi) ) {
        //round to whole or .5 number
        float auxVal = fabs(val);
        float baseInt = trunc(auxVal);
        printf("%s - %g trunked to %g\n", __func__, auxVal, baseInt);
        if (auxVal - baseInt < 0.25)
            auxVal = baseInt;
        else if (auxVal - baseInt < 0.75)
            auxVal = baseInt + 0.5;
        else
            auxVal = baseInt + 1;
        if (val < 0)
            auxVal = - auxVal;
        setKomi(auxVal);
    }
}

void Handicap::handicapSelected(int value) {
    printf("%s - handicap set to %d\n", __func__, value);
    if (value < 1)
        handicap.handicap = 0;
    else
        handicap.handicap = value + 1;
}

void Handicap::handicapPlacementSelected(int value) {
    printf("%s - handicap placement %d\n", __func__, value);
    if (value == 0)
        handicap.handicapPlacementFree = false;
    else
        handicap.handicapPlacementFree = true;
}

void Handicap::populateHandicap(SGameSettings::Handicap aHandicap) {
    handicap = aHandicap;
    setKomi(handicap.komi);
    if (handicap.handicap < 2)
        ui->handicapCombo->setCurrentIndex(0);
    else
        ui->handicapCombo->setCurrentIndex(handicap.handicap-1);
    if (handicap.handicapPlacementFree)
        ui->handicapPlacementCombo->setCurrentIndex(1);
    else
        ui->handicapPlacementCombo->setCurrentIndex(0);
}
