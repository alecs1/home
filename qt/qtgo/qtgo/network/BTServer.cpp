#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothServer>
#include <QtBluetooth/QBluetoothDeviceDiscoveryAgent>

#include "../Global.h"

#include "BTServer.h"
#include "ConnMan.h"

//TODO - replace this, it comes from the Qt example
static const QLatin1String serviceUuid("e8e10f95-1a70-4b27-9ccf-02010264e9c8");

QString qtgoUUID("7a17c611-7857-48d9-95e3-ab56df7e5af2");


BTServer::BTServer(ConnMan *connMan) : connMan(connMan)
{

}

BTServer::~BTServer() {
    delete rfcommServer;
    //delete discoveryAgent; //delete through QObject means
}

//TODO - the code asking the user to activate bluetooth needs to be run each time button is pressed!
int BTServer::initBluetooth() {

    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    printf("%s - localAdapters.count=%d\n", __PRETTY_FUNCTION__, localAdapters.count());

    if (localAdapters.size() < 1) {
        printf("%s - no bluetooth found\n", __PRETTY_FUNCTION__);
        return -1;
    }

    QBluetoothLocalDevice adapter(localAdapters.at(0).address());
    adapter.powerOn();
    adapter.setHostMode(QBluetoothLocalDevice::HostDiscoverable);

    rfcommServer = new QBluetoothServer(QBluetoothServiceInfo::RfcommProtocol, NULL);
    connect(rfcommServer, SIGNAL(newConnection()), this, SLOT(clientConnected()));

    QBluetoothAddress actualAddress = localAdapters[0].address();
    printf("%s - QBluetoothServer - starting to listen at %s\n", __PRETTY_FUNCTION__,
           actualAddress.toString().toUtf8().constData());
    bool result = rfcommServer->listen(actualAddress);
    if (!result) {
        printf("%s - listen failed; result=%d\n", __PRETTY_FUNCTION__, result);
        return -2;
    }

    //! [Get local device name]
    QString localName = QBluetoothLocalDevice().name();
    printf("%s - local device name=%s\n", __PRETTY_FUNCTION__, localName.toLocal8Bit().constData());


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
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceName, tr("FreeGo p2p"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceDescription,
                             tr("FreeGo peer server"));
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
        printf("%s - registering with SDP failed\n", __PRETTY_FUNCTION__);
        return -3;
    }

    scanForDevices();

    printf("%s - success\n", __PRETTY_FUNCTION__);
    return 0;
}

//this is actually a client thing
int BTServer::scanForDevices() {
    if (discoveryAgent == NULL) {
        discoveryAgent = new QBluetoothDeviceDiscoveryAgent(this);
        connect(discoveryAgent, SIGNAL(deviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(deviceDiscovered(QBluetoothDeviceInfo)));
    }
    discoveryAgent->start();
    printf("%s - done\n", __PRETTY_FUNCTION__);
    return 0;
}

QList<BTPeerInfo> BTServer::getPeers() {
    return peers;
}

int BTServer::connectAddress(const QString& address) {
    printf("%s - connecting to %s\n", __PRETTY_FUNCTION__, address.toUtf8().constData());
    for(int i = 0; i < peers.size(); i++) {
        if (address == peers[i].address) {
            QBluetoothSocket* socket = new QBluetoothSocket(QBluetoothServiceInfo::RfcommProtocol, this);
            socket->connectToService(peers[i].deviceInfo.address(), QBluetoothUuid(qtgoUUID));
            printf("%s - connected to %s\n", __PRETTY_FUNCTION__, address.toUtf8().constData());
        }
    }
    return 0;
}

void BTServer::clientConnected() {
    printf("%s - a client has connected\n", __PRETTY_FUNCTION__);
    connMan->processMessage(nullptr);
}

void BTServer::deviceDiscovered(QBluetoothDeviceInfo deviceInfo) {
    printf("%s - address:%s, name:%s, signal strength:%d\n",
           __PRETTY_FUNCTION__,
           deviceInfo.address().toString().toUtf8().constData(),
           deviceInfo.name().toUtf8().constData(),
           deviceInfo.rssi());

//    QBluetoothSocket* socket = new QBluetoothSocket(QBluetoothServiceInfo::RfcommProtocol, this);
//    if (deviceInfo.name() == "debian" || deviceInfo.name() == "Motorola Defy" || deviceInfo.name() == "Xperia Z1 Compact") {
//        socket->connectToService(deviceInfo.address(), QBluetoothUuid(qtgoUUID));
//        printf("%s - discovered an expected device\n", __PRETTY_FUNCTION__);
//    }

    BTPeerInfo p;
    p.deviceInfo = deviceInfo;
    p.name = deviceInfo.name();
    p.address = deviceInfo.address().toString();
    p.strength = deviceInfo.rssi();
    peers.append(p);

    //printf("%s - socket->state=%d\n", __PRETTY_FUNCTION__, socket->state());
}




