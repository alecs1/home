#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothServer>

#include "DebugStuff.h"
#include "Global.h"

DebugStuff::DebugStuff()
{

}
int DebugStuff::runBTCode() {

    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    printf("%s - localAdapters.count=%d\n", __func__, localAdapters.count());
    for(int i = 0; i < localAdapters.size(); i++) {
        printf("%s- adapter %d: %s, %s\n", __func__, i,
               localAdapters[i].name().toUtf8().constData(),
               localAdapters[i].address().toString().toUtf8().constData());
    }

    if (localAdapters.size() < 1)
        return -1;

    QBluetoothLocalDevice adapter(localAdapters.at(0).address());
    adapter.setHostMode(QBluetoothLocalDevice::HostDiscoverable);



    QBluetoothServer *rfcommServer = new QBluetoothServer(QBluetoothServiceInfo::RfcommProtocol, NULL);
    //connect(rfcommServer, SIGNAL(newConnection()), this, SLOT(clientConnected()));

    QBluetoothAddress actualAddress = localAdapters[0].address();
    printf("%s - QBluetoothServer - starting to listen at %s\n", __func__,
           actualAddress.toString().toUtf8().constData());
    bool result = rfcommServer->listen(actualAddress);
    printf("%s - listen result:%d\n", __func__, result);

    //server->startServer(localAdapters[0].address());
    //! [Create Chat Server]

    //! [Get local device name]
    QString localName = QBluetoothLocalDevice().name();
    printf("%s - local device name=%s\n", __func__, localName.toLocal8Bit().constData());

    return 0;
}

