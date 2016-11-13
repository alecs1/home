#include "PeerChooser.h"
#include "ui_PeerChooser.h"

#include <QtBluetooth/QBluetoothHostInfo>

#include "BTServer.h"
#include "PeerWidget.h"
#include "BTErrorDialog.h"
#include "../Logger.h"

PeerChooser::PeerChooser(ConnMan &connectionManager, QWidget *parent) :
    connMan(connectionManager),
    QDialog(parent),
    ui(new Ui::PeerChooser)
{
    ui->setupUi(this);
    connect(ui->listWidget, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated(const QModelIndex&)));
    connect(ui->reloadButton, SIGNAL(clicked()), this, SLOT(rescan()));
    connect(ui->bt1Button, SIGNAL(toggled(bool)), this, SLOT(chooseBT0(bool)));
    connect(ui->bt2Button, SIGNAL(toggled(bool)), this, SLOT(chooseBT1(bool)));
    connect(&connectionManager, SIGNAL(newDeviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(peerFound()));
    connect(&connectionManager, SIGNAL(finishedScanning()), this, SLOT(finishedScanning()));

    BTServer* btServer = connMan.getBTServer();
    QList<QBluetoothHostInfo> devices = btServer->getBTDevices();
    if (devices.size() > 0) {
        ui->bt1Button->setText(devices[0].name() + "(" + devices[0].address().toString() + ")");
    }

    if (devices.size() > 1) {
        ui->bt2Button->setText(devices[1].name() + "(" + devices[1].address().toString() + ")");
    }

    //if there's only one device we use it, if there are more we expect user input
    if (devices.size() == 1) {
        int ret = btServer->initBluetooth(0);
        if (ret == -1) {
            BTErrorDialog dialog("Error initialising bluetooth:\n" + QString::number(ret));
            dialog.exec();
        }
        rescan();
    }

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

    connMan.getBTServer()->connectAddress(p->address());
    close();
}

void PeerChooser::chooseBT0(bool chosen) {
    if (chosen) {
        printf("%s\n", __func__);
        if (chosenBTIf != 0) {
            chosenBTIf = 0;
            connMan.getBTServer()->initBluetooth(chosenBTIf);
        }
    }
}

void PeerChooser::chooseBT1(bool chosen) {
    if (chosen) {
        printf("%s\n", __func__);
        if (chosenBTIf != 1) {
            chosenBTIf = 1;
            connMan.getBTServer()->initBluetooth(chosenBTIf);
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
    QList<BTPeerInfo> peers = connMan.getBTServer()->getPeers();
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
    Logger::log(QString("%1").arg(__PRETTY_FUNCTION__));
    //if BT interface has not been chosen at scan time, choose the first one.
    if (chosenBTIf == -1) {
        chosenBTIf = 0;
        connMan.getBTServer()->initBluetooth(0);
    }
    connMan.getBTServer()->scanBTPeers();
    displayPeers();
}
