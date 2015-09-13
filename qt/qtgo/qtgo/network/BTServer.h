#ifndef BTSERVER_H
#define BTSERVER_H

#include <QObject>
#include <QtBluetooth/QBluetoothServiceInfo>
#include <QtBluetooth/QBluetoothDeviceInfo>

class QBluetoothServer;
class ConnMan;
class QBluetoothSocket;
class QBluetoothDeviceDiscoveryAgent;

class BTServer : public QObject
{
Q_OBJECT
public:
    BTServer(ConnMan* connMan);
    ~BTServer();
    int initBluetooth();
    int startBluetoothDiscovery();

private slots:
    void clientConnected();
    void deviceDiscovered(QBluetoothDeviceInfo deviceInfo);

private:
    QBluetoothServer *rfcommServer = NULL;
    QBluetoothServiceInfo serviceInfo;
    QBluetoothDeviceDiscoveryAgent* discoveryAgent = NULL;
    QList<QBluetoothSocket *> clientSockets; //only one for starters
    ConnMan* connMan;
};

#endif // BTSERVER_H
