#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include <QDialog>

class SettingsWidget;

namespace Ui {
class SettingsDialog;
}

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SettingsDialog(QWidget *parent = 0);
    ~SettingsDialog();

private:
    Ui::SettingsDialog *ui;
    SettingsWidget* settingsWidget;

};

#endif // SETTINGSDIALOG_H
