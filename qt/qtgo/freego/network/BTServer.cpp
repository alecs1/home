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
    Logger::log(QString("%1 - interfaceNo=%2").arg(__PRETTY_FUNCTION__).arg(interfaceNo));

    //reinitialise everything
    delete rfcommServer;
    if (discoveryAgent != nullptr) {
        discoveryAgent->stop();
        delete discoveryAgent;
        discoveryAgent = nullptr;
    }
    serviceInfo.unregisterService();


    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    Logger::log(QString("%1 - localAdapters.count=%2").arg(__PRETTY_FUNCTION__).arg(localAdapters.count()));

    if (localAdapters.size() < 1) {
        Logger::log(QString("%1 - no bluetooth found").arg(__PRETTY_FUNCTION__));
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
    Logger::log(QString("%1 - QBluetoothServer - starting to listen at %2").arg(__PRETTY_FUNCTION__).arg(actualAddress.toString()), Logger::DBG);
    bool result = rfcommServer->listen(actualAddress);
    if (!result) {
        Logger::log(QString("%1 - listen failed; result=%2").arg(__PRETTY_FUNCTION__).arg(result), Logger::ERR);
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
        Logger::log(QString("%1 - registering with SDP failed").arg(__PRETTY_FUNCTION__), Logger::ERR);
        return -3;
    }

    discoveryAgent = new QBluetoothDeviceDiscoveryAgent(adapter.address());
    connect(discoveryAgent, SIGNAL(deviceDiscovered(QBluetoothDeviceInfo)), this, SLOT(peerDeviceDiscovered(QBluetoothDeviceInfo)));
    connect(discoveryAgent, SIGNAL(finished()), this, SIGNAL(finishedScanning()));
    scanBTPeers();

    Logger::log(QString("%1 - success").arg(__PRETTY_FUNCTION__), Logger::DBG);
    return 0;
}

//this is actually a client thing
int BTServer::scanBTPeers() {
    Logger::log(QString("%1 - enter").arg(__PRETTY_FUNCTION__), Logger::DBG);
    if (discoveryAgent) {
        peers.clear();
        discoveryAgent->stop();
        discoveryAgent->start();
    }
    else {
        Logger::log(QString("%1 - device not initialised!").arg(__PRETTY_FUNCTION__), Logger::ERR);
        return -1;
    }
    Logger::log(QString("%1 - done").arg(__PRETTY_FUNCTION__), Logger::DBG);
    return 0;
}

QList<BTPeerInfo> BTServer::getPeers() {
    return peers;
}

//client thing
int BTServer::connectAddress(const QString& address) {
    Logger::log(QString("%1 - start connecting to %2").arg(__PRETTY_FUNCTION__).arg(address), Logger::DBG);
    for(int i = 0; i < peers.size(); i++) {
        if (address == peers[i].address) {
            socket->connectToService(peers[i].deviceInfo.address(), QBluetoothUuid(qtgoUUID));
            Logger::log(QString("%1 - called connectToService for %2").arg(__PRETTY_FUNCTION__).arg(address));
        }
    }
    return 0;
}

void BTServer::clientConnected() {
    Logger::log(QString("%1 - a client has connected").arg(__PRETTY_FUNCTION__), Logger::INFO);
    QBluetoothSocket* newSock = rfcommServer->nextPendingConnection();
    clientSockets.append(newSock);
    connMan->setBTServerSocket(newSock);
}

void BTServer::socketConnected() {
    Logger::log(QString("%1 - enter").arg(__PRETTY_FUNCTION__), Logger::DBG);
    connMan->setBTClientSocket(socket);
}

void BTServer::socketDisconnected() {
    Logger::log(QString("%1 - enter").arg(__PRETTY_FUNCTION__), Logger::DBG);
}

void BTServer::socketError(QBluetoothSocket::SocketError error) {
    Logger::log(QString("%1 : %2 -> %3").arg(__PRETTY_FUNCTION__).arg(error).arg(socket->errorString()), Logger::ERR);
}

void BTServer::peerDeviceDiscovered(QBluetoothDeviceInfo deviceInfo) {
    Logger::log(QString("%1 - address:%2, name:%3, signal strength:%4").arg(__PRETTY_FUNCTION__).arg(deviceInfo.address().toString()).arg(deviceInfo.name()).arg(deviceInfo.rssi()), Logger::DBG);

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




