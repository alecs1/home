#ifndef THIRDPARTYINFO_H
#define THIRDPARTYINFO_H

#include <QDialog>

class QLabel;

class ThirdPartyInfo : public QDialog {
public:
    ThirdPartyInfo(QWidget* parent);

private:
    void populateAckLabel();

private:
    QPushButton* okButton;
    QLabel* ackLabel;
};

#endif // THIRDPARTYINFO_H

