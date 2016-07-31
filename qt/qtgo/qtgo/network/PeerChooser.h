#ifndef PEERCHOOSER_H
#define PEERCHOOSER_H

#include <QDialog>

#include "BTServer.h"

namespace Ui {
class PeerChooser;
}

class PeerChooser : public QDialog {
Q_OBJECT

public:
    explicit PeerChooser(BTServer& bt, QWidget *parent = 0);
    ~PeerChooser();

public slots:
    void activated(const QModelIndex &index);
    void rescan();
    void chooseBT0(bool chosen);
    void chooseBT1(bool chosen);
    void peerFound();
    void finishedScanning();

private:
    void displayPeers();

private:
    BTServer& btServer;
    Ui::PeerChooser *ui;
    int chosenBTIf = -1;
};

#endif // PEERCHOOSER_H
