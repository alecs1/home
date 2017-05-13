#pragma once

#include <QString>
#include <QJsonObject>


#include "Global.h"
#include "Settings.h"

//TODO - try to get rid of this include
extern "C" {
#include <sgf/sgftree.h>
}

class SaveFile
{
public:
    static QString qetDefSaveFName();
    static bool loadSave(QByteArray data, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool loadSave(QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QByteArray& data, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static QJsonObject serialiseGameState(SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static QString getGnuGoSaveString(SGFNode* sgfNode);

    static QString getDefSettingsFName();
    static bool loadSettings(QString settingsFName, SProgramSettings* programSettings);
    static bool writeSettings(QString settingsFName, SProgramSettings* programSettings);
};
