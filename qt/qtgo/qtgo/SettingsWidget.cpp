#include <QColorDialog>

#include "SettingsWidget.h"
#include "ui_SettingsWidget.h"
#include "Utils.h"
#include "SaveFile.h"

SettingsWidget::SettingsWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SettingsWidget)
{
    ui->setupUi(this);

    settings = *Settings::getProgramSettings();
    populateSettings();

    connect(ui->colourButton, SIGNAL(clicked()), this, SLOT(showColourDialog()));
    //TODO - play sound at each move, to give feedback on the sound volume.
    connect(ui->soundVolumeSlider, SIGNAL(valueChanged(int)), this, SLOT(applySettings()));
    connect(ui->spaceOptimisationCheck, SIGNAL(stateChanged(int)), this, SLOT(applySettings()));
    connect(ui->revertSoundsButton, SIGNAL(clicked(bool)), this, SLOT(setDefaultSounds()));
    connect(ui->revertColourButton, SIGNAL(clicked(bool)), this, SLOT(setDefaultColour()));
    connect(ui->revertSpaceOptimisationButton, SIGNAL(clicked(bool)), this, SLOT(setDefaultSpaceOptimisations()));


    //TODO - hack because we don't actually control volume yet.
    ui->soundVolumeSlider->setSingleStep(SProgramSettings::maxSoundsVolume);
    ui->soundVolumeSlider->setMaximum(SProgramSettings::maxSoundsVolume);
    printf("%s - singleStep:%d, maximum:%d\n", __func__, ui->soundVolumeSlider->singleStep(), ui->soundVolumeSlider->maximum());

    int qToolButtonMinSize = Utils::estimateQToolButtonSize();
    QList<QToolButton*> qToolButtons;
    qToolButtons.append(ui->colourButton);
    qToolButtons.append(ui->revertColourButton);
    qToolButtons.append(ui->revertSoundsButton);
    qToolButtons.append(ui->revertSpaceOptimisationButton);
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
    resetDefaultButtons.append(ui->revertSpaceOptimisationButton);

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
    colorDialog.setCurrentColor(settings.tableColour);

    if (colorDialog.exec() == QDialog::Accepted) {
        QColor colour = colorDialog.selectedColor();
        settings.tableColour = colour.name();
        QString colourStyleSheet = "background-color: " + colour.name() + ";";
        ui->colourButton->setStyleSheet(colourStyleSheet);
        applySettings();
    }

}

void SettingsWidget::populateSettings() {
    QColor colour(settings.tableColour);
    ui->soundVolumeSlider->setValue(settings.soundsVolume);
    QString colourStyleSheet = "background-color: " + colour.name() + ";";
    ui->colourButton->setStyleSheet(colourStyleSheet);
    ui->spaceOptimisationCheck->setChecked(settings.spaceOptimisations);
}

void SettingsWidget::readSettings() {
    settings.soundsVolume = ui->soundVolumeSlider->value();
    settings.spaceOptimisations = ui->spaceOptimisationCheck->isChecked();
    printf("%s - soundVolume=%d\n", __func__, settings.soundsVolume);
}

void SettingsWidget::applySettings() {
    readSettings();
    *Settings::getProgramSettings() = settings;
    SaveFile::writeSettings(SaveFile::getDefSettingsFName(), &settings);
    Settings::notifyReloadProgramSettings();
}

void SettingsWidget::setDefaultColour() {
    SProgramSettings aux;
    Settings::populateDefaultProgramSettings(&aux);
    settings.tableColour = aux.tableColour;
    populateSettings();
    applySettings();
}

void SettingsWidget::setDefaultSounds() {
    SProgramSettings aux;
    Settings::populateDefaultProgramSettings(&aux);
    settings.soundsVolume = aux.soundsVolume;
    populateSettings();
    applySettings();
}

void SettingsWidget::setDefaultSpaceOptimisations() {
    SProgramSettings aux;
    Settings::populateDefaultProgramSettings(&aux);
    settings.spaceOptimisations = aux.spaceOptimisations;
    populateSettings();
    applySettings();
}

void SettingsWidget::show() {
    QWidget::show();
    //need to show up to compute good icons sizes
    printf("%s - toolButton size:%d\n", __func__, ui->revertColourButton->width());
}

