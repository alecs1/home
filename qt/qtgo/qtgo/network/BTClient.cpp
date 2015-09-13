#include "BTClient.h"

#include <QtBluetooth/QBluetoothDeviceDiscoveryAgent>

BTClient::BTClient(QObject *parent) : QObject(parent)
{

}

void BTClient::startClient() {
    startBluetoothDiscovery();
}

int BTClient::startBluetoothDiscovery() {
    if (discoveryAgent == NULL) {
        discoveryAgent = new QBluetoothDeviceDiscoveryAgent(this);
        connect(discoveryAgent, SIGNAL(deviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(deviceDiscovered(QBluetoothDeviceInfo)));
    }
    discoveryAgent->start();
    printf("%s - done\n", __func__);
    return 0;
}

void BTClient::deviceDiscovered(QBluetoothDeviceInfo deviceInfo) {
    printf("%s - address:%s, name:%s, signal strength:%d\n",
           __func__,
           deviceInfo.address().toString().toUtf8().constData(),
           deviceInfo.name().toUtf8().constData(),
           deviceInfo.rssi());

    //is it running a QtGo server? at this point just connect to it
}

