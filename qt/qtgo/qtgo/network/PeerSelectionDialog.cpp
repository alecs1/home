#include "PeerSelectionDialog.h"
#include "ui_PeerSelectionDialog.h"

PeerSelectionDialog::PeerSelectionDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PeerSelectionDialog)
{
    ui->setupUi(this);
}

PeerSelectionDialog::~PeerSelectionDialog()
{
    delete ui;
}

int PeerSelectionDialog::addPeerInfo(QString& peer) {

}
