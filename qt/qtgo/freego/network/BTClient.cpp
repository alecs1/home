#include "BTClient.h"

#include <QtBluetooth/QBluetoothDeviceDiscoveryAgent>
#include <QtBluetooth/QBluetoothServiceInfo>
#include <QtBluetooth/QBluetoothSocket>

#include "../Logger.h"


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
    Logger::log(QString("%1 - address:%2, name:%3, signal strength:%4").arg(__PRETTY_FUNCTION__).arg(deviceInfo.address().toString()).arg(deviceInfo.name()).arg(deviceInfo.rssi()));

    //is it running a QtGo server? at this point just connect to it
    if (deviceInfo.name() == "debian" || deviceInfo.name() == "Motorola Defy" || deviceInfo.name() == "Xperia Z1 Compact") {
        socket->connectToService(deviceInfo.address(), QBluetoothUuid(qtgoUUID));
        Logger::log(QString("%1 - discovered an expected device").arg(__PRETTY_FUNCTION__));
    }

    Logger::log(QString("%1 - socket->state=%2").arg(__PRETTY_FUNCTION__).arg(socket->state()));
}

