#include "SaveFile.h"

#include <stdint.h>
#include <QJsonObject>
#include <QJsonDocument>
#include <QFile>
#include <QCryptographicHash>
#include <QTextStream>
#include <QRegularExpression>
#include <QTemporaryFile>

#include "Logger.h"

extern "C" {
#include "sgftree.h"
}


/*!
    \class SaveFile
    \brief The SaveFile class does stuff.
*/

QString SaveFile::qetDefSaveFName() {
    return "FreeGoCrt.autosave";
}

bool SaveFile::loadSave(const QJsonObject json, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
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

    QTemporaryFile auxFile("temp-stream-for-gnugo.XXXXXX.txt");
    if (!auxFile.open()) {
        Logger::log("Failed to open temporary file.", LogLevel::ERR);
        return false;
    }
    QByteArray bytes = SGFSaveString.toUtf8();
    auxFile.write(bytes);
    auxFile.flush();
    FILE* auxFStream = fopen(auxFile.fileName().toUtf8().constData(), "rb");
    *sgfNode = readsgfStream(auxFStream); //fclose is called by readsgfStream
    auxFile.remove();

    return true;
}

bool SaveFile::loadSave(const QByteArray data, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
    QJsonDocument jsonDoc = QJsonDocument::fromJson(data);
    QJsonObject json = jsonDoc.object();
    return loadSave(json, sgfNode, gameSettings, auxGameInfo);
}

/*!
 * \brief loadSave - wrapper of the other loadSaves.
 */
bool SaveFile::loadSave(const QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile inFile(saveFName);
    if (!inFile.exists())
        return false;

    inFile.open(QIODevice::ReadOnly);
    QByteArray saveData = inFile.readAll();
    bool success = loadSave(saveData, sgfNode, gameSettings, auxGameInfo);

    return success;
}

bool SaveFile::writeSave(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
    json = serialiseGameState(sgfNode, gameSettings, auxGameInfo);
    addValidationData(json, sgfNode, gameSettings, auxGameInfo);
    return true;
}

bool SaveFile::writeSave(QByteArray& data, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
    QJsonObject json;
    writeSave(json, sgfNode, gameSettings, auxGameInfo);
    QJsonDocument doc;
    doc.setObject(json);
    data = doc.toJson(QJsonDocument::Indented);
    return true;
}

bool SaveFile::writeSave(QString saveFName, SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo *auxGameInfo) {
    QFile outFile(saveFName);
    QByteArray contents;
    writeSave(contents, sgfNode, gameSettings, auxGameInfo);
    outFile.open(QIODevice::WriteOnly);
    outFile.write(contents);
    outFile.close();
    return true;
}

/*!
 * \brief serialise to be read by a remote player
 */
bool SaveFile::writeSaveForRemote(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
    json = serialiseGameState(sgfNode, gameSettings, auxGameInfo);

    if (gameSettings->white == PlayerType::LocalHuman) {
        QJsonObject remote = json["black"];
        remote["type"] = PlayerType::Network;
        json["black"] = remote; //is this necessary?
    }
    else if (gameSettings->black == PlayerType::LocalHuman) {
        QJsonObject remote = json["white"];
        remote["type"] = PlayerType::Network;
        json["white"] = remote;
    }
    else {
        Logger::log("Cannot decide which player should be the remote one! Need at least a human player!");
        return false;
    }

    return true;
}

QJsonObject SaveFile::serialiseGameState(SGFNode *sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
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

    QString SGFSaveString = getGnuGoSaveString(sgfNode);
    json["SGFSaveString"] = SGFSaveString;
    return json;
}

void SaveFile::addValidationData(QJsonObject& json, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo) {
    //hash some stuff to validate the save file;
    QString contentsToHash = auxGameInfo->comment + auxGameInfo->freeGoVersion +
            auxGameInfo->gameDate + json["SGFSaveString"].toString();

    json["hashedStuff"] = "comment+freeGoVersion+gameDate+SGFSaveString";
    QByteArray hashBytes = QCryptographicHash::hash(contentsToHash.toUtf8(), QCryptographicHash::Md5);
    json["hashMD5"] = hashBytes.toHex().constData();
}

QString SaveFile::getGnuGoSaveString(SGFNode* sgfNode) {
    //TODO - this can be achieved directly in memory buffers, no need for real files
    QString SGFSaveString;
    QString auxSaveFName = "gnugo-serialise.tmp";
    FILE* auxFile = fopen(auxSaveFName.toUtf8().constData(), "w+"); //fclose by QTextStream destructor
    writesgfToStream(sgfNode, auxFile);

    QTextStream fileStream(auxFile);
    fileStream.seek(0);
    SGFSaveString = fileStream.readAll();
    SGFSaveString.remove(QRegularExpression("\r|\n"));
    return SGFSaveString;
}

QString SaveFile::getDefSettingsFName() {
    return "FreeGoSettings.config";
}

bool SaveFile::loadSettings(QString settingsFName, SProgramSettings *programSettings) {
    QFile inFile(settingsFName);
    if (!inFile.exists())
        return false;

    inFile.open(QIODevice::ReadOnly);
    QByteArray saveData = inFile.readAll();

    QJsonDocument jsonDoc = QJsonDocument::fromJson(saveData);
    QJsonObject json = jsonDoc.object();
    programSettings->soundsVolume = json["soundsVolume"].toInt();
    programSettings->tableColour = json["tableColour"].toString();
    programSettings->spaceOptimisations = json["spaceOptimisations"].toBool();
    programSettings->minimalInterface = json["minimalInterface"].toBool();


    QString wantedHashVal = json["hashMD5"].toString();
    QString contentsToHash = json["soundsVolume"].toString() + json["tableColour"].toString()
            + json["spaceOptimisations"].toString();
    QByteArray hashBytes = QCryptographicHash::hash(contentsToHash.toUtf8(), QCryptographicHash::Md5);
    QString hashVal(hashBytes.toHex().constData());

    if (! (hashVal == wantedHashVal)) {
        Logger::log(QString("%1 - hash %2 does not match %3").arg(__func__).arg(wantedHashVal).arg(hashVal), LogLevel::ERR);
        return false;
    }

    return true;
}

bool SaveFile::writeSettings(QString settingsFName, SProgramSettings* gameSettings) {
    QFile outFile(settingsFName);

    QJsonObject json;
    json["soundsVolume"] = (int)gameSettings->soundsVolume;
    json["tableColour"] = gameSettings->tableColour;
    json["spaceOptimisations"] = gameSettings->spaceOptimisations;
    json["minimalInterface"] = gameSettings->minimalInterface;

    //hash some stuff to validate the save file;
    QString contentsToHash = json["soundsVolume"].toString() + json["tableColour"].toString()
            + json["spaceOptimisations"].toString();

    //printf("%s - contentsToHash:->%s<-\n", __func__, contentsToHash.toUtf8().constData());
    json["hashedStuff"] = "soundsVolume+tableColour+soundsVolume";
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
