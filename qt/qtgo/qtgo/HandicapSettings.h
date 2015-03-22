#ifndef HANDICAPSETTINGS_H
#define HANDICAPSETTINGS_H

#include <QDialog>

namespace Ui {
class HandicapSettings;
}

class HandicapSettings : public QDialog
{
    Q_OBJECT

public:
    explicit HandicapSettings(QWidget *parent = 0);
    ~HandicapSettings();

private:
    Ui::HandicapSettings *ui;
};

#endif // HANDICAPSETTINGS_H
