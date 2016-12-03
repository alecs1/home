#ifndef PEERSELECTIONDIALOG_H
#define PEERSELECTIONDIALOG_H

#include <QDialog>

namespace Ui {
class PeerSelectionDialog;
}

class PeerSelectionDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PeerSelectionDialog(QWidget *parent = 0);
    int addPeerInfo(QString& peer);
    ~PeerSelectionDialog();

private:
    Ui::PeerSelectionDialog *ui;
    QStringList peers;
};

#endif // PEERSELECTIONDIALOG_H
