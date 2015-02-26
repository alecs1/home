#ifndef SAVEFILE_H
#define SAVEFILE_H

#include <QString>
#include "Global.h"

class SaveFile
{
public:
    SaveFile();
    ~SaveFile();
    QString qetDefSaveFName() const;
    static bool loadSave(QString saveFName, SGFNode **sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);
    static bool writeSave(QString saveFName, SGFNode* sgfNode, SGameSettings* gameSettings, SAuxGameInfo* auxGameInfo);

private:
    QString defSaveFName = "FreeGoCrt.json";
};

#endif // SAVEFILE_H
