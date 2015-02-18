#include <QJsonObject>

#include "SaveFile.h"

SaveFile::SaveFile()
{

}

SaveFile::~SaveFile()
{

}

QString SaveFile::qetDefSaveFName() const {
    return defSaveFName;
}

static bool SaveFile::loadSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {

}

static bool SaveFile::writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile outFile(saveFName);

    QJsonObject json;
    json["comment"] = auxGameInfo->comment;
    json["freeGoVersion"] = auxGameInfo->freeGoVersion;
    json["gameDate"] = auxGameInfo->gameDate;

    json["tableSize"] = gameSettings->
}

