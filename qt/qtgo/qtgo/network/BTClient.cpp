#include "BTClient.h"

#include <QtBluetooth/QBluetoothDeviceDiscoveryAgent>
#include <QtBluetooth/QBluetoothServiceInfo>
#include <QtBluetooth/QBluetoothSocket>


QString qtgoUUID("7a17c611-7857-48d9-95e3-ab56df7e5af2");

BTClient::BTClient(QObject *parent) : QObject(parent)
{
    socket = new QBluetoothSocket(QBluetoothServiceInfo::RfcommProtocol, this);
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
    printf("%s - done\n", __PRETTY_FUNCTION__ );
    return 0;
}

void BTClient::deviceDiscovered(QBluetoothDeviceInfo deviceInfo) {
    printf("%s - address:%s, name:%s, signal strength:%d\n",
           __PRETTY_FUNCTION__,
           deviceInfo.address().toString().toUtf8().constData(),
           deviceInfo.name().toUtf8().constData(),
           deviceInfo.rssi());

    //is it running a QtGo server? at this point just connect to it
    if (deviceInfo.name() == "debian" || deviceInfo.name() == "Motorola Defy" || deviceInfo.name() == "Xperia Z1 Compact") {
        socket->connectToService(deviceInfo.address(), QBluetoothUuid(qtgoUUID));
        printf("%s - discovered an expected device\n", __PRETTY_FUNCTION__);
    }

    printf("%s - socket->state=%d\n", __PRETTY_FUNCTION__, socket->state());

}

