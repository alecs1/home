#ifndef HELPDIALOG_H
#define HELPDIALOG_H

#include <QDialog>

class QTextEdit;

class HelpDialog : public QDialog {
Q_OBJECT
public:
    HelpDialog(QWidget* parent=0);
    //~HelpDialog();

private:
    QTextEdit* contents = NULL;
    QPushButton* okButton;
};

#endif // HELPDIALOG_H
