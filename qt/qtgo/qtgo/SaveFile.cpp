#include <QJsonObject>
#include <QFile>

#include "sgftree.h"

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

bool SaveFile::loadSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {

}

bool SaveFile::writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile outFile(saveFName);

    QJsonObject json;
    json["comment"] = auxGameInfo->comment;
    json["freeGoVersion"] = auxGameInfo->freeGoVersion;
    json["gameDate"] = auxGameInfo->gameDate;

    json["tableSize"] = gameSettings->tableSize;
    json["estimateScore"] = gameSettings->estimateScore;
}

