#include "SettingsDialog.h"
#include "ui_SettingsDialog.h"

#include "SettingsWidget.h"

SettingsDialog::SettingsDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::SettingsDialog)
{
    ui->setupUi(this);
    settingsWidget = new SettingsWidget(this);
    ui->verticalLayout->insertWidget(0, settingsWidget);
    //ui->buttonBox->
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}
