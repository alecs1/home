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
    //TODO - some of these loadSave/writeSave are duplicated, should disappear
    static bool loadSave(const QJsonObject json, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool loadSave(const QByteArray data, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool loadSave(const QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool loadSaveFromRemote(const QJsonObject json, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QByteArray& data, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSaveForRemote(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);

    static QJsonObject serialiseGameState(SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static void addValidationData(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);

    static QString getGnuGoSaveString(SGFNode* sgfNode);

    static QString getDefSettingsFName();
    static bool loadSettings(QString settingsFName, SProgramSettings* programSettings);
    static bool writeSettings(QString settingsFName, SProgramSettings* programSettings);
};
