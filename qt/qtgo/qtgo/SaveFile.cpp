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

bool SaveFile::loadSave(QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile inFile(saveFName);
    if (!inFile.exists())
        return false;

    inFile.open(QIODevice::ReadOnly);

    QByteArray saveData = inFile.readAll();

    QJsonDocument jsonDoc = QJsonDocument::fromJson(saveData);
    QJsonObject json = jsonDoc.object();

    auxGameInfo->comment = json["comment"].toString();
    auxGameInfo->freeGoVersion = json["freeGoVersion"].toString();
    auxGameInfo->gameDate = json["gameDate"].toString();

    gameSettings->size = json["tableSize"].toInt();
    gameSettings->estimateScore = json["estimateScore"].toBool();

    gameSettings->white = (PlayerType)json["white"].toObject()["type"].toInt();
    gameSettings->whiteAIStrength = json["white"].toObject()["AILevel"].toInt();
    gameSettings->black = (PlayerType)json["black"].toObject()["type"].toInt();
    gameSettings->blackAIStrength = json["black"].toObject()["AILevel"].toInt();

    QString wantedHashVal = json["hashMD5"].toString();

    QString SGFSaveString = json["SGFSaveString"].toString();

    QString contentsToHash = auxGameInfo->comment + auxGameInfo->freeGoVersion + auxGameInfo->gameDate + SGFSaveString;
    QByteArray hashBytes = QCryptographicHash::hash(contentsToHash.toUtf8(), QCryptographicHash::Md5);
    QString hashVal(hashBytes.toHex().constData());

    if (! (hashVal == wantedHashVal)) {
        printf("%s - hash %s does not match %s\n",
               __func__, wantedHashVal.toUtf8().constData(), hashVal.toUtf8().constData());
        return false;
    }

    QString auxSaveFName = saveFName + "tmp";
    FILE* auxFile = fopen(auxSaveFName.toUtf8().constData(), "w+"); //fclose is called by readsgfStream
    QByteArray bytes = SGFSaveString.toUtf8();
    fwrite(bytes.constData(), bytes.size(), 1, auxFile);
    fseek(auxFile, 0, SEEK_SET);
    *sgfNode = readsgfStream(auxFile);

    return true;
}

bool SaveFile::writeSave(QString saveFName, SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile outFile(saveFName);

    QJsonObject json;
    json["comment"] = auxGameInfo->comment;
    json["freeGoVersion"] = auxGameInfo->freeGoVersion;
    json["gameDate"] = auxGameInfo->gameDate;

    json["tableSize"] = gameSettings->size;
    json["estimateScore"] = gameSettings->estimateScore;

    QJsonObject white;
    white["type"] = (int)gameSettings->white;
    white["AILevel"] = gameSettings->whiteAIStrength;
    json["white"] = white;

    QJsonObject black;
    black["type"] = (int)gameSettings->black;
    black["AILevel"] = gameSettings->blackAIStrength;
    json["black"] = black;

    QString SGFSaveString;
    QString auxSaveFName = saveFName + "tmp";
    FILE* auxFile = fopen(auxSaveFName.toUtf8().constData(), "w+"); //fclose by QTextStream destructor
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
    QByteArray hashBytes = QCryptographicHash::hash(contentsToHash.toUtf8(), QCryptographicHash::Md5);
    json["hashMD5"] = hashBytes.toHex().constData();

    QJsonDocument doc;
    doc.setObject(json);
    QByteArray contents = doc.toJson(QJsonDocument::Indented);
    outFile.open(QIODevice::WriteOnly);
    outFile.write(contents);
    outFile.close();

    return true;
}

