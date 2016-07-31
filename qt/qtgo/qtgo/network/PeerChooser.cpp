#include "PeerChooser.h"
#include "network/ui_PeerChooser.h"

#include "BTServer.h"
#include "PeerWidget.h"
#include <QtBluetooth/QBluetoothHostInfo>

PeerChooser::PeerChooser(BTServer &bt, QWidget *parent) :
    btServer(bt),
    QDialog(parent),
    ui(new Ui::PeerChooser)
{
    ui->setupUi(this);
    connect(ui->listWidget, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated(const QModelIndex&)));
    connect(ui->reloadButton, SIGNAL(clicked()), this, SLOT(rescan()));
    connect(ui->bt1Button, SIGNAL(toggled(bool)), this, SLOT(chooseBT0(bool)));
    connect(ui->bt2Button, SIGNAL(toggled(bool)), this, SLOT(chooseBT1(bool)));
    connect(&bt, SIGNAL(newDeviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(peerFound()));
    connect(&bt, SIGNAL(finishedScanning()), this, SLOT(finishedScanning()));

    QList<QBluetoothHostInfo> devices = bt.getBTDevices();
    if (devices.size() > 0) {
        ui->bt1Button->setText(devices[0].name() + "(" + devices[0].address().toString() + ")");
    }

    if (devices.size() > 1) {
        ui->bt2Button->setText(devices[1].name() + "(" + devices[1].address().toString() + ")");
    }

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

void PeerChooser::chooseBT0(bool chosen) {
    if (chosen) {
        printf("%s\n", __func__);
        if (chosenBTIf != 0) {
            chosenBTIf = 0;
            btServer.initBluetooth(chosenBTIf);
        }
    }
}

void PeerChooser::chooseBT1(bool chosen) {
    if (chosen) {
        printf("%s\n", __func__);
        if (chosenBTIf != 1) {
            chosenBTIf = 1;
            btServer.initBluetooth(chosenBTIf);
        }
    }
}
void PeerChooser::peerFound() {
    displayPeers();
}

void PeerChooser::finishedScanning() {
    displayPeers();
}

void PeerChooser::displayPeers() {
    QList<BTPeerInfo> peers = btServer.getPeers();
    ui->listWidget->clear();

    for(int i = 0; i < peers.size(); i++) {
        PeerWidget* p = new PeerWidget(ConnType::ConnBT, peers[i].name, peers[i].address);
        p->setStrength(peers[i].strength);
        QListWidgetItem* aux = new QListWidgetItem(ui->listWidget);
        aux->setSizeHint(p->size());
        ui->listWidget->setItemWidget(aux, p);
    }
}

void PeerChooser::rescan() {
    btServer.scanBTPeers();
    displayPeers();
}
