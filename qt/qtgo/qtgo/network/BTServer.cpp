#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothServer>
#include <QtBluetooth/QBluetoothDeviceDiscoveryAgent>

#include "../Global.h"

#include "BTServer.h"
#include "ConnMan.h"
#include "ProtoJson.h"
#include "../Logger.h"

//TODO - replace this, it comes from the Qt example
static const QLatin1String serviceUuid("e8e10f95-1a70-4b27-9ccf-02010264e9c8");

QString qtgoUUID("7a17c611-7857-48d9-95e3-ab56df7e5af2");


BTServer::BTServer(ConnMan *connMan) :
    connMan(connMan)
{
    socket = new QBluetoothSocket(QBluetoothServiceInfo::RfcommProtocol, this);
    connect(socket, SIGNAL(connected()), this, SLOT(socketConnected()));
    connect(socket, SIGNAL(disconnected()), this, SLOT(socketDisconnected()));
    connect(socket, SIGNAL(error(QBluetoothSocket::SocketError)), this, SLOT(socketError(QBluetoothSocket::SocketError)));
}

BTServer::~BTServer() {
    delete rfcommServer;
}

/**
 * @return the list of BT devices installed in computer
 */
QList<QBluetoothHostInfo> BTServer::getBTDevices() const {
    return QBluetoothLocalDevice::allDevices();
}

//TODO - the code asking the user to activate bluetooth needs to be run each time button is pressed!
int BTServer::initBluetooth(const int interfaceNo) {
    //reinitialise everything
    delete rfcommServer;
    if (discoveryAgent != nullptr) {
        discoveryAgent->stop();
        delete discoveryAgent;
        discoveryAgent = nullptr;
    }
    serviceInfo.unregisterService();


    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    printf("%s - localAdapters.count=%d\n", __PRETTY_FUNCTION__, localAdapters.count());

    if (localAdapters.size() < 1) {
        printf("%s - no bluetooth found\n", __PRETTY_FUNCTION__);
        return -1;
    }

    int interface = interfaceNo;
    if (interface >= localAdapters.size()) {
        interface = 0;
    }

    QBluetoothLocalDevice adapter(localAdapters.at(interface).address());
    adapter.powerOn();
    adapter.setHostMode(QBluetoothLocalDevice::HostDiscoverable);

    rfcommServer = new QBluetoothServer(QBluetoothServiceInfo::RfcommProtocol, nullptr);
    connect(rfcommServer, SIGNAL(newConnection()), this, SLOT(clientConnected()));

    QBluetoothAddress actualAddress = localAdapters.at(interface).address();
    printf("%s - QBluetoothServer - starting to listen at %s\n", __PRETTY_FUNCTION__,
           actualAddress.toString().toUtf8().constData());
    bool result = rfcommServer->listen(actualAddress);
    if (!result) {
        printf("%s - listen failed; result=%d\n", __PRETTY_FUNCTION__, result);
        return -2;
    }


    {
#pragma region Attributes
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceRecordHandle, (uint)0x00010010);

    //! [Class Uuuid must contain at least 1 entry]
    QBluetoothServiceInfo::Sequence classId;

    classId << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::SerialPort));
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,
                             classId);


    classId.prepend(QVariant::fromValue(QBluetoothUuid(qtgoUUID)));

    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceClassIds, classId);
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,classId);
    //! [Class Uuuid must contain at least 1 entry]

    //! [Service name, description and provider]
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceName, tr("FreeGo p2p"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceDescription,
                             tr("FreeGo peer server"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceProvider, tr("qt-project.org"));


    //! [Service UUID set]
    serviceInfo.setServiceUuid(QBluetoothUuid(qtgoUUID));


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
    }

    //! [Register service]
    result = serviceInfo.registerService(actualAddress);
    if (!result) {
        printf("%s - registering with SDP failed\n", __PRETTY_FUNCTION__);
        return -3;
    }

    discoveryAgent = new QBluetoothDeviceDiscoveryAgent(adapter.address());
    connect(discoveryAgent, SIGNAL(deviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(peerDeviceDiscovered(QBluetoothDeviceInfo)));
    connect(discoveryAgent, SIGNAL(finished()), this, SIGNAL(finishedScanning()));
    scanBTPeers();

    printf("%s - success\n", __PRETTY_FUNCTION__);
    return 0;
}

//this is actually a client thing
int BTServer::scanBTPeers() {
    printf("%s - enter\n", __PRETTY_FUNCTION__);
    if (discoveryAgent) {
        peers.clear();
        discoveryAgent->stop();
        discoveryAgent->start();
    }
    else {
        printf("%s - error, device not initialised!\n", __PRETTY_FUNCTION__);
        return -1;
    }
    printf("%s - done\n", __PRETTY_FUNCTION__);
    return 0;
}

QList<BTPeerInfo> BTServer::getPeers() {
    return peers;
}

//client thing
int BTServer::connectAddress(const QString& address) {
    printf("%s - start connecting to %s\n", __PRETTY_FUNCTION__, address.toUtf8().constData());
    for(int i = 0; i < peers.size(); i++) {
        if (address == peers[i].address) {

            socket->connectToService(peers[i].deviceInfo.address(), QBluetoothUuid(qtgoUUID));
            printf("%s - called connectToService for %s\n", __PRETTY_FUNCTION__, address.toUtf8().constData());
        }
    }
    return 0;
}

void BTServer::clientConnected() {
    printf("%s - a client has connected\n", __PRETTY_FUNCTION__);
    QBluetoothSocket* newSock = rfcommServer->nextPendingConnection();
    clientSockets.append(newSock);
    connMan->setBTServerSocket(newSock);
}

void BTServer::socketConnected() {
    printf("%s - enter\n", __PRETTY_FUNCTION__);
    ProtoJson::Msg handshake = ProtoJson::Msg::composeHandshake();

    QByteArray data = ProtoJson::Msg::serialise(handshake);
    socket->write(data);
    Logger::log(QString("%1 - wrote \"%2\" to socket\n").arg(__PRETTY_FUNCTION__).arg(data.constData()));
}

void BTServer::socketDisconnected() {
    printf("%s - enter\n", __PRETTY_FUNCTION__);
}

void BTServer::socketError(QBluetoothSocket::SocketError error) {
    printf("%s - error:%d -> %s\n", __PRETTY_FUNCTION__, error, socket->errorString().toUtf8().constData());
}

void BTServer::peerDeviceDiscovered(QBluetoothDeviceInfo deviceInfo) {
    Logger::log(QString("%1 - address:%2, name:%3, signal strength:%4\n").arg(__PRETTY_FUNCTION__).arg(deviceInfo.address().toString().toUtf8().constData()).arg(deviceInfo.name().toUtf8().constData()).arg(deviceInfo.rssi()));

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

    emit newDeviceDiscovered(deviceInfo);
    //printf("%s - socket->state=%d\n", __PRETTY_FUNCTION__, socket->state());
}




