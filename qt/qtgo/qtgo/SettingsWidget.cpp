#include <QColorDialog>

#include "SettingsWidget.h"
#include "ui_SettingsWidget.h"
#include "Utils.h"

//globals:
#include "GoTable.h"
GoTable* table = NULL;
void settingsSetGoTable(GoTable* aTable) {
    table = aTable;
}

bool applyProgramSettings(SProgramSettings* newSettings) {
    if (table == NULL) {
        return false;
    }

    table->changeProgramSettings(newSettings);
    return true;
}

void populateDefaultProgramSettings(SProgramSettings* defaults) {
    QColor defaultColour(206, 170, 57, 255);
    defaults->tableColour = defaultColour.rgba();

    defaults->soundsVolume = 100;
}

bool loadProgramSettings(SProgramSettings* settings) {

}

bool writeProgramSettings(SProgramSettings* settings) {

}

bool getProgramSettings(SProgramSettings* settings) {
    *settings = *(table->getProgramSettings());
}

//end global settings

SettingsWidget::SettingsWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SettingsWidget)
{
    ui->setupUi(this);

    connect(ui->colourButton, SIGNAL(clicked()), this, SLOT(showColourDialog()));
    //TODO - play sound at each move, to give feedback on the sound volume.
    connect(ui->soundVolumeSlider, SIGNAL(valueChanged(int)), this, SLOT(applySettings()));
    connect(ui->revertSoundsButton, SIGNAL(clicked(bool)), this, SLOT(setDefaultSounds()));
    connect(ui->revertColourButton, SIGNAL(clicked(bool)), this, SLOT(setDefaultColour()));
    getProgramSettings(&settings);
    populateSettings();

    //TODO - hack because we don't actually control volume yet.
    ui->soundVolumeSlider->setSingleStep(SProgramSettings::maxSoundsVolume);
    ui->soundVolumeSlider->setMaximum(SProgramSettings::maxSoundsVolume);
    printf("%s - singleStep:%d, maximum:%d\n", __func__, ui->soundVolumeSlider->singleStep(), ui->soundVolumeSlider->maximum());

    int qToolButtonMinSize = Utils::estimateQToolButtonSize();
    QList<QToolButton*> qToolButtons;
    qToolButtons.append(ui->colourButton);
    qToolButtons.append(ui->revertColourButton);
    qToolButtons.append(ui->revertSoundsButton);
    for (int i = 0; i < qToolButtons.size(); i++) {
        qToolButtons[i]->setMinimumWidth(qToolButtonMinSize);
        qToolButtons[i]->setMinimumHeight(qToolButtonMinSize);
    }
    ui->colourButton->setMinimumWidth(5 * qToolButtonMinSize);

    //QSvg

    QIcon setDefaultIcon(QString(":/resources/defaultValue--edit-undo.svg"));
    QList<QToolButton*> resetDefaultButtons;
    resetDefaultButtons.append(ui->revertColourButton);
    resetDefaultButtons.append(ui->revertSoundsButton);

    for (int i = 0; i < resetDefaultButtons.size(); i++) {
        resetDefaultButtons[i]->setIcon(setDefaultIcon);
        resetDefaultButtons[i]->setIconSize(QSize(qToolButtonMinSize * 0.8, qToolButtonMinSize * 0.8));
    }

}

SettingsWidget::~SettingsWidget()
{
    delete ui;
}

void SettingsWidget::showColourDialog() {
    QColorDialog colorDialog;

    if (colorDialog.exec() == QDialog::Accepted) {
        QColor colour = colorDialog.selectedColor();
        settings.tableColour = colour.rgba();
        QString colourStyleSheet = "background-color: " + colour.name() + ";";
        ui->colourButton->setStyleSheet(colourStyleSheet);
        applySettings();
    }

}

void SettingsWidget::populateSettings() {
    QColor colour(settings.tableColour);
    //ui->soundsCheckBox->setChecked(settings.useSounds);
    ui->soundVolumeSlider->setValue(settings.soundsVolume);
    QString colourStyleSheet = "background-color: " + colour.name() + ";";
    ui->colourButton->setStyleSheet(colourStyleSheet);
}

void SettingsWidget::readSettings() {
    settings.soundsVolume = ui->soundVolumeSlider->value();
    settings.soundsVolume = ui->soundVolumeSlider->value();
    printf("%s - soundVolume=%d\n", __func__, settings.soundsVolume);
}

void SettingsWidget::applySettings() {
    readSettings();
    applyProgramSettings(&settings);
}

void SettingsWidget::setDefaultColour() {
    SProgramSettings aux;
    populateDefaultProgramSettings(&aux);
    settings.tableColour = aux.tableColour;
    populateSettings();
    applySettings();
}

void SettingsWidget::setDefaultSounds() {
    SProgramSettings aux;
    populateDefaultProgramSettings(&aux);
    settings.soundsVolume = aux.soundsVolume;
    populateSettings();
    applySettings();
}

void SettingsWidget::show() {
    QWidget::show();
    //need to show up to compute good icons sizes
    printf("%s - toolButton size:%d\n", __func__, ui->revertColourButton->width());
}

