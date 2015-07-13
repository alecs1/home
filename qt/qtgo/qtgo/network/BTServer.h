#ifndef BTSERVER_H
#define BTSERVER_H

#include <QObject>
#include <QtBluetooth/QBluetoothServiceInfo>

class QBluetoothServer;

class BTServer : public QObject
{
Q_OBJECT
public:
    BTServer();
    int initBluetooth();

private:
    void clientConnected();

private:
    QBluetoothServer *rfcommServer;
    QBluetoothServiceInfo serviceInfo;
};

#endif // BTSERVER_H
