#ifndef SAVEFILE_H
#define SAVEFILE_H

#include <QString>
#include "Global.h"
#include "Settings.h"

class SaveFile
{
public:
    //SaveFile();
    //~SaveFile();
    static QString qetDefSaveFName();
    static bool loadSave(QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);

    static QString getDefSettingsFName();
    static bool loadSettings(QString settingsFName, SProgramSettings* programSettings);
    static bool writeSettings(QString settingsFName, SProgramSettings* programSettings);
};

#endif // SAVEFILE_H
