#include <QJsonObject>
#include <QJsonDocument>
#include <QFile>
#include <QCryptographicHash>
#include <QTextStream>
#include <QRegularExpression>

//#include <stdio.h> //for

extern "C" {
#include "sgftree.h"
}

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
    QFile inFile(saveFName);

    inFile.open(QIODevice::ReadOnly);

    QByteArray saveData = inFile.readAll();

    QJsonDocument jsonDoc = QJsonDocument::fromJson(saveData);
    QJsonObject json = jsonDoc.object();

    auxGameInfo->comment = json["comment"].toString();
    auxGameInfo->freeGoVersion = json["freeGoVersion"].toString();
    auxGameInfo->gameDate = json["gameDate"].toString();

    gameSettings->tableSize = json["tableSize"].toInt();
    gameSettings->estimateScore = json["estimateScore"].toBool();

    return true;
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
    QString auxSaveFName = saveFName + "tmp";
    FILE* auxFile = fopen(auxSaveFName.toUtf8().constData(), "w+");
    writesgfToStream(sgfNode, auxFile);

    QTextStream fileStream(auxFile);
    fileStream.seek(0);
    SGFSaveString = fileStream.readAll();
    SGFSaveString.remove(QRegularExpression("\r|\n"));
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

    return true;
}

