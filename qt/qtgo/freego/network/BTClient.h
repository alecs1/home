#pragma once

#include <QObject>
#include <QtBluetooth/QBluetoothDeviceInfo>
class QBluetoothDeviceDiscoveryAgent;

class QBluetoothSocket;

class BTClient : public QObject
{
    Q_OBJECT
public:
    explicit BTClient(QObject *parent = 0);

    void startClient();
    int startBluetoothDiscovery();

signals:

private slots:
    void deviceDiscovered(QBluetoothDeviceInfo deviceInfo);

private:
    QBluetoothSocket* socket;
    QBluetoothDeviceDiscoveryAgent* discoveryAgent = NULL;
};
