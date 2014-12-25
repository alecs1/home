#include "GameSettings.h"
#include "ui_GameSettings.h"

GameSettings::GameSettings(QWidget *parent):
    ui(new Ui::GameSettings())
{
    ui->setupUi(this);

    connect(ui->launchButton, SIGNAL(clicked()), this, SLOT(launchGameClicked()));

    populateSettings();
}

void GameSettings::setGameState(GameState state) {
    if (state == GameState::Started) {
        ui->launchButton->setText("Finish");
        ui->tableSizeGroupBox->setEnabled(false);
    }
    else if (state == GameState::Stopped) {
        ui->launchButton->setText("Start");
        ui->tableSizeGroupBox->setEnabled(true);
    }
}

bool operator==(const SGameSettings& s1, const SGameSettings& s2) {
    if (s1.AILevel != s2.AILevel)
        return false;
    if (s1.size != s2.size)
        return false;
    return true;
}

void GameSettings::populateSettings() {
    printf("%s\n", __func__);
    SGameSettings newSettings;
    newSettings.size = 19;
    if (ui->button9x9->isChecked())
        newSettings.size = 9;
    else if (ui->button13x13->isChecked())
        newSettings.size = 13;
    else if (ui->button19x19->isChecked())
        newSettings.size = 19;

    if (newSettings == settings)
        return;
    else {
        printf("%s - settings have changed\n", __func__);
        settings = newSettings;
        //emit settingsChanged(settings);
    }
}

void GameSettings::launchGameClicked() {
    populateSettings();
    emit launchGamePerform(settings);
}

/*
void GameSettings::keyReleaseEvent(QKeyEvent * event) {
    populateSettings();
    QWidget::keyReleaseEvent(event);
}

void GameSettings::mouseReleaseEvent(QMouseEvent * event) {
    populateSettings();
    QWidget::mouseReleaseEvent(event);
}
*/
