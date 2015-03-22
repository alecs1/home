#include "HandicapSettings.h"
#include "ui_HandicapSettings.h"

HandicapSettings::HandicapSettings(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HandicapSettings)
{
    ui->setupUi(this);
    ui->komiEdit->hide();
    connect(ui->komiInputButton, SIGNAL(clicked()), ui->komiEdit, SLOT(show()));
}

HandicapSettings::~HandicapSettings()
{
    delete ui;
}
