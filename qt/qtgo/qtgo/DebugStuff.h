#ifndef DEBUGSTUFF_H
#define DEBUGSTUFF_H

#include <QtBluetooth/QBluetoothServiceInfo>


class DebugStuff : public QObject
{
Q_OBJECT

public:
    DebugStuff();

    int runBTCode();
    int initBluetooth();

public slots:
    void clientConnected();

private:
    QBluetoothServiceInfo serviceInfo;
};

#endif // DEBUGSTUFF_H
