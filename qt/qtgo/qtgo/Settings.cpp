#include <QColor>

#include "Settings.h"

SProgramSettings Settings::settings;
SProgramSettings* Settings::getProgramSettings() {
    return &(Settings::settings);
}

void Settings::populateDefaultProgramSettings(SProgramSettings* defaults) {
    QColor defaultColour(206, 170, 57, 255);
    defaults->tableColour = defaultColour.name();
    defaults->soundsVolume = 100;
}
