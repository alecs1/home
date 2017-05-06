#pragma once

#include <QString>
#include "Global.h"
#include "Settings.h"

//TODO - try to get rid of this include
#include <sgf/sgftree.h>

class SaveFile
{
public:
    //SaveFile();
    //~SaveFile();
    static QString qetDefSaveFName();
    static bool loadSave(QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static QString getSaveString(SGFNode* sgfNode);

    static QString getDefSettingsFName();
    static bool loadSettings(QString settingsFName, SProgramSettings* programSettings);
    static bool writeSettings(QString settingsFName, SProgramSettings* programSettings);
};
