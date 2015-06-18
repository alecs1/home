#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include <QWidget>

#include "Global.h"
#include "Settings.h"

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
    void setDefaultSpaceOptimisations();
    void show();

private:
    void populateSettings();
    void readSettings();

private:
    Ui::SettingsWidget *ui;
    SProgramSettings settings;
};

#endif // SETTINGSWIDGET_H
