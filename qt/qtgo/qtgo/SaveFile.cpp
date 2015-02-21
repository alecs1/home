#include <QJsonObject>
#include <QJsonDocument>
#include <QFile>
#include <QCryptographicHash>

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

bool SaveFile::loadSave(QString saveFName, SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {

}

bool SaveFile::writeSave(QString saveFName, SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile outFile(saveFName);

    QJsonObject json;
    json["comment"] = auxGameInfo->comment;
    json["freeGoVersion"] = auxGameInfo->freeGoVersion;
    json["gameDate"] = auxGameInfo->gameDate;

    json["tableSize"] = gameSettings->tableSize;
    json["estimateScore"] = gameSettings->estimateScore;

    QString SGFSaveString;
    json["SGFSaveString"] = SGFSaveString;

    //hash some stuff to validate the save file;
    QString contentsToHash = auxGameInfo->comment + auxGameInfo->freeGoVersion +
            auxGameInfo->gameDate + SGFSaveString;
    printf("%s - contentsToHash:->%s<-\n", __func__, contentsToHash.toUtf8().constData());
    json["hashedStuff"] = "comment+freeGoVersion+gameDate+SGFSaveString";
    QByteArray hashVal = QCryptographicHash::hash(contentsToHash.toUtf8(), QCryptographicHash::Md5);
    json["hashMD5"] = hashVal.toHex().constData();

    QJsonDocument doc;
    doc.setObject(json);
    QByteArray contents = doc.toJson(QJsonDocument::Indented);
    outFile.open(QIODevice::WriteOnly);
    outFile.write(contents);
    outFile.close();
}

