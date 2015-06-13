#include "Settings.h"

SProgramSettings Settings::settings;
SProgramSettings* Settings::getProgramSettings() {
    return &(Settings::settings);
}
