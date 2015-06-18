#include <QColor>

#include "Settings.h"

#include "mainwindow.h"

SProgramSettings Settings::settings;
MainWindow* Settings::messageSender = NULL;
SProgramSettings* Settings::getProgramSettings() {
    return &(Settings::settings);
}

void Settings::populateDefaultProgramSettings(SProgramSettings* defaults) {
    QColor defaultColour(206, 170, 57, 255);
    defaults->tableColour = defaultColour.name();
    defaults->soundsVolume = 100;
}

/*
 * MainWindow is the one sending messages using signals and slots.
 * Only let this file know about the existance of a MainWindow;
 */
bool Settings::notifyReloadSettings() {
    if (messageSender != NULL) {
        messageSender->notifyReloadSettings();
        return true;
    }
    return false;
}

void Settings::setMessageSender(MainWindow *newSender) {
    messageSender = newSender;
}
