#ifndef BTSERVER_H
#define BTSERVER_H

#include <QObject>
#include <QtBluetooth/QBluetoothServiceInfo>

class QBluetoothServer;
class ConnMan;

class BTServer : public QObject
{
Q_OBJECT
public:
    BTServer(ConnMan* connMan);
    int initBluetooth();

private:
    void clientConnected();

private:
    QBluetoothServer *rfcommServer;
    QBluetoothServiceInfo serviceInfo;
    ConnMan* connMan;
};

#endif // BTSERVER_H
