#include "GameChooser.h"
#include "ui_GameChooser.h"

GameChooser::GameChooser(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::GameChooser)
{
    ui->setupUi(this);
}

GameChooser::~GameChooser()
{
    delete ui;
}
