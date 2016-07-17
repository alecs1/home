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


private:
    BTServer& btServer;
    Ui::PeerChooser *ui;
};

#endif // PEERCHOOSER_H
