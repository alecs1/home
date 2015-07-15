#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothServer>

#include "../Global.h"

#include "BTServer.h"
#include "ConnMan.h"

//TODO - replace this, it comes from the Qt example
static const QLatin1String serviceUuid("e8e10f95-1a70-4b27-9ccf-02010264e9c8");


BTServer::BTServer(ConnMan *connMan) : connMan(connMan)
{

}


int BTServer::initBluetooth() {

    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    printf("%s - localAdapters.count=%d\n", __func__, localAdapters.count());

    if (localAdapters.size() < 1) {
        printf("%s - no bluetooth found\n", __func__);
        return -1;
    }

    QBluetoothLocalDevice adapter(localAdapters.at(0).address());
    adapter.setHostMode(QBluetoothLocalDevice::HostDiscoverable);

    rfcommServer = new QBluetoothServer(QBluetoothServiceInfo::RfcommProtocol, NULL);
    connect(rfcommServer, SIGNAL(newConnection()), this, SLOT(clientConnected()));

    QBluetoothAddress actualAddress = localAdapters[0].address();
    printf("%s - QBluetoothServer - starting to listen at %s\n", __func__,
           actualAddress.toString().toUtf8().constData());
    bool result = rfcommServer->listen(actualAddress);
    if (!result) {
        printf("%s - listen failed; result=%d\n", __func__, result);
        return -2;
    }

    //! [Get local device name]
    QString localName = QBluetoothLocalDevice().name();
    printf("%s - local device name=%s\n", __func__, localName.toLocal8Bit().constData());


#pragma region Attributes
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceRecordHandle, (uint)0x00010010);

    //! [Class Uuuid must contain at least 1 entry]
    QBluetoothServiceInfo::Sequence classId;

    classId << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::SerialPort));
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,
                             classId);


    classId.prepend(QVariant::fromValue(QBluetoothUuid(serviceUuid)));

    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceClassIds, classId);
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,classId);
    //! [Class Uuuid must contain at least 1 entry]

    //! [Service name, description and provider]
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceName, tr("Bt Chat Server"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceDescription,
                             tr("Example bluetooth chat server"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceProvider, tr("qt-project.org"));


    //! [Service UUID set]
    serviceInfo.setServiceUuid(QBluetoothUuid(serviceUuid));


    //! [Service Discoverability]
    QBluetoothServiceInfo::Sequence publicBrowse;
    publicBrowse << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::PublicBrowseGroup));
    serviceInfo.setAttribute(QBluetoothServiceInfo::BrowseGroupList,
                             publicBrowse);

    //! [Protocol descriptor list]
    QBluetoothServiceInfo::Sequence protocolDescriptorList;
    QBluetoothServiceInfo::Sequence protocol;
    protocol << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::L2cap));
    protocolDescriptorList.append(QVariant::fromValue(protocol));
    protocol.clear();
    protocol << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::Rfcomm))
             << QVariant::fromValue(quint8(rfcommServer->serverPort()));
    protocolDescriptorList.append(QVariant::fromValue(protocol));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ProtocolDescriptorList,
                             protocolDescriptorList);

    #pragma endregion

    //! [Register service]
    result = serviceInfo.registerService(actualAddress);
    if (!result) {
        printf("%s - registering with SDP failed\n", __func__);
        return -3;
    }

    printf("%s - done\n", __func__);
    return 0;
}

void BTServer::clientConnected() {
    printf("%s - a client has connected\n", __func__);
    connMan->processMessage(NULL);
}


