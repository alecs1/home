#ifndef BTSERVER_H
#define BTSERVER_H

#include <QObject>
#include <QtBluetooth/QBluetoothServiceInfo>

class QBluetoothServer;
class ConnMan;
class QBluetoothSocket;

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
    QList<QBluetoothSocket *> clientSockets; //only one for starters
    ConnMan* connMan;
};

#endif // BTSERVER_H
