#include "PeerChooser.h"
#include "network/ui_PeerChooser.h"

#include "BTServer.h"
#include "PeerWidget.h"

PeerChooser::PeerChooser(BTServer &bt, QWidget *parent) :
    btServer(bt),
    QDialog(parent),
    ui(new Ui::PeerChooser)
{
    ui->setupUi(this);
    connect(ui->listWidget, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated(const QModelIndex&)));
    connect(ui->reloadButton, SIGNAL(clicked()), this, SLOT(rescan()));

    rescan();
}

PeerChooser::~PeerChooser() {
    delete ui;
}


void PeerChooser::activated(const QModelIndex& index) {
    PeerWidget* p = dynamic_cast<PeerWidget*>(ui->listWidget->indexWidget(index));
    if (p == nullptr) {
        printf("%s - dynamic_cast failed!\n", __PRETTY_FUNCTION__);
        return;
    }

    btServer.connectAddress(p->address());
    close();
}

void PeerChooser::rescan() {
    btServer.scanForDevices();
    QList<BTPeerInfo> peers = btServer.getPeers();
    ui->listWidget->clear();

    for(int i = 0; i < peers.size(); i++) {
        PeerWidget* p = new PeerWidget(ConnType::ConnBT, peers[i].name, peers[i].address);
        p->setStrength(peers[i].strength);
        QListWidgetItem* aux = new QListWidgetItem(ui->listWidget);
        ui->listWidget->setItemWidget(aux, p);
    }
}
