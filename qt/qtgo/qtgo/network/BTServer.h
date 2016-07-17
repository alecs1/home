#ifndef BTSERVER_H
#define BTSERVER_H

#include <QObject>
#include <QtBluetooth/QBluetoothServiceInfo>
#include <QtBluetooth/QBluetoothDeviceInfo>

class QBluetoothServer;
class ConnMan;
class QBluetoothSocket;
class QBluetoothDeviceDiscoveryAgent;

struct BTPeerInfo {
    QBluetoothDeviceInfo deviceInfo;
    QString address;
    QString name;
    int strength;
};

class BTServer : public QObject
{
Q_OBJECT
public:
    BTServer(ConnMan* connMan);
    ~BTServer();
    int initBluetooth();
    int scanForDevices();
    QList<BTPeerInfo> getPeers();
    int connectAddress(const QString& address);

private slots:
    void clientConnected();
    void deviceDiscovered(QBluetoothDeviceInfo deviceInfo);

private:
    QBluetoothServer *rfcommServer = nullptr;
    QBluetoothServiceInfo serviceInfo;
    //TODO - discoveryAgent and client logic should go to the BTClient class
    QBluetoothDeviceDiscoveryAgent* discoveryAgent = nullptr;
    QList<QBluetoothSocket *> clientSockets; //only one for starters
    ConnMan* connMan;
    QList<BTPeerInfo> peers;
};

#endif // BTSERVER_H
