#ifndef SETTINGS_H
#define SETTINGS_H

#include <QString>

#include "Global.h"

struct SGameSettings {
    int size = 19;
    int blackAIStrength = 0;
    int whiteAIStrength = 0;
    PlayerType black = PlayerType::LocalHuman;
    PlayerType white = PlayerType::AI;
    struct Handicap {
        float komi = 6.5;
        int handicap = 0;
        bool handicapPlacementFree = false;
    };
    Handicap handicap;

    //information used for saving
    bool estimateScore;

    //game state, move to another struct if there are more elements
    int crtPlayer;
};

struct SProgramSettings {
    static const int maxSoundsVolume = 1; //hack, we only accept 0 or 1 for now
    uint32_t soundsVolume = 100;
    QString tableColour;
};

//Extra information about a game, don't know if this is the right place
struct SAuxGameInfo {
    QString comment;
    QString freeGoVersion;
    QString gameDate;
};


class Settings {
public:
    static SProgramSettings* getProgramSettings();
    static void populateDefaultProgramSettings(SProgramSettings* defaults);
private:
    static SProgramSettings settings;
    //static SG
};

#endif // SETTINGS_H

