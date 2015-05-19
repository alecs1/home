#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include <QWidget>

#include "Global.h"

//A few global functions and variables, move them somewhere else when they accumulate
//blocking
class GoTable;
void settingsSetGoTable(GoTable *aTable);
bool applyProgramSettings(SProgramSettings* newSettings);
void populateDefaultProgramSettings(SProgramSettings* defaults);
//load from default save file
bool loadProgramSettings(SProgramSettings* settings);
//write to default save file
bool writeProgramSettings(SProgramSettings* settings);
//get program settings from table
bool getProgramSettings(SProgramSettings* settins);



namespace Ui {
class SettingsWidget;
}

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    explicit SettingsWidget(QWidget *parent = 0);
    ~SettingsWidget();

public slots:
    void showColourDialog();
    void applySettings();
    void setDefaultColour();
    void setDefaultSounds();
    void show();

private:
    void populateSettings();
    void readSettings();

private:
    Ui::SettingsWidget *ui;
    SProgramSettings settings;
};

#endif // SETTINGSWIDGET_H
