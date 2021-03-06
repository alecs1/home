#include "ConnMan.h"

#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QTcpServer>
#include <QtBluetooth/QBluetoothSocket>

#include "BTServer.h"
#include "ProtoJson.h"
#include "../Logger.h"
#include "../MainWindow.h" //TODO - this is only temporary

using namespace ProtoJson;

//See for a way to choose these ports: https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers#Registered_ports
const uint16_t tcpDefaultFirstPort = 1030;
const uint16_t tcpDefaultLastPort = 1050;

ConnMan::ConnMan(MainWindow *gameManager) : gameManager(gameManager) {
    btServer = new BTServer(this);
    tcpServer = new QTcpServer(this);

    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(newConnectionTCP()));
}

ConnMan::~ConnMan() {

}

void ConnMan::connectBT(const QString address) {
    btServer->connectAddress(address);
    connState = ConnState::AwaitingHandshakeReply;
}

/**
 * Listen on all interfaces, both IPv4 and IPv6.
 * Port chosen from the predefined range.
 */
uint16_t ConnMan::listenTCP() {
    if (tcpServer->isListening()) {
        Logger::log(QString("Will stop listening on port %1 and start listening on a port in predefined range %2-%3").arg(tcpServer->serverPort()).arg(tcpDefaultFirstPort).arg(tcpDefaultLastPort));
    }

    int port = tcpDefaultFirstPort;
    bool success = false;
    while (!success && port <= tcpDefaultLastPort) {
        success = tcpServer->listen(QHostAddress(QHostAddress::Any), port);
        port++;
    }

    if (success) {
        Logger::log(QString("Started listening on port %1").arg(tcpServer->serverPort()));
    }
    else {
        Logger::log("Could not listen on any of the interfaces.", Logger::ERR);
    }

    return tcpServer->serverPort();
}

/**
 * Try to connect on TCP, currently blocks trying on a port range
 * @param address - if missing it will default to localhost
 * @param port - if missing default to the entire default range
 */
bool ConnMan::connectTCP(const QString address/* = ""*/, const uint16_t port/* = 0*/) {
    const int timeout = 5000;

    QHostAddress addr(QHostAddress::Any);
    if (address.length() > 0) {
        addr = QHostAddress(address);
    }

    uint64_t firstPort = tcpDefaultFirstPort;
    uint64_t lastPort = tcpDefaultLastPort;
    if (port > 0) {
        firstPort = port;
        lastPort = port;
    }

    Logger::log(QString("%1 - %2:%3-%4").arg(LOG_POS).arg(addr.toString()).arg(firstPort).arg(lastPort));

    bool success = false;
    uint16_t crtPort = firstPort;
    QTcpSocket* sock = new QTcpSocket();
    while (!success && crtPort <= lastPort) {
        Logger::log(QString("Compare %1 %2").arg(addr.toString()).arg(tcpServer->serverAddress().toString()));
        if (addr.isEqual(tcpServer->serverAddress()) ||
           (addr.isEqual(QHostAddress(QHostAddress::LocalHost)) && tcpServer->serverAddress().isEqual(QHostAddress::Any)))
        {
            Logger::log(QString("Addresses: %1 %2").arg(addr.toIPv4Address()).arg(tcpServer->serverAddress().toIPv4Address()));
            if (crtPort == tcpServer->serverPort()) {
                //skip ourselves
                crtPort += 1;
            }
        }
        sock->connectToHost(addr, crtPort);
        if (sock->waitForConnected(timeout)) {
            Logger::log(QString("Connected as client to: %1:%2").arg(addr.toIPv4Address()).arg(crtPort));
            success = true;
        }
        else {
            crtPort += 1;
        }
    }

    if (success) {
        Logger::log(QString("Connected as client. Socket %1:%2 <-> %3:%4").arg(sock->localAddress().toString()).arg(sock->localPort()).arg(sock->peerAddress().toString()).arg(sock->peerPort()));
        assert(!tcpSocket);
        tcpSocket = sock;
        connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));
        connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(socketDisconnected()));
        initClientState();
    }
    else {
        Logger::log(QString("%1 - failed to connect, last error: %2").arg(LOG_POS).arg(sock->errorString()));
        delete sock;
    }
    return success;
}

BTServer* ConnMan::getBTServer() const {
    return btServer;
}

/**
 * @brief ConnMan::setBTClientSocket set the socket to a client connection, we're behaving as server
 */
void ConnMan::setBTClientSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connect(btSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));
    initServerState();
}

/**
 * @brief ConnMan::setBTServerSocket set the socket to a server connection, we're behaving as a client
 */
void ConnMan::setBTServerSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connect(btSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));
}

/**
 * Read the next message from buffer, if available
 * @param lenght of the message read
 * @return
 */
ProtoJson::Msg ConnMan::getMessage(int& parsedBytes) {
    parsedBytes = 0;
    if (buffer.size() >= Msg::HEADER_LEN) {
        ProtoJson::Msg ret = Msg::parse(buffer, parsedBytes);
        if (Msg::msgValid(ret)) {
            buffer.remove(0, parsedBytes);
            return ret;
       }
    }
    return ProtoJson::Msg();
}

void ConnMan::update() {
    if (!socket()) {
        return;
    }

    if (buffer.size() > 0) {
        int len = 0;
        ProtoJson::Msg msg = getMessage(len);
        if (len > 0) {
            switch (connState) {
                case ConnState::AwaitingHandshake: {
                    if (msg.type == MsgType::Hanshake) {
                        connState = ConnState::Connected;
                        Logger::log(QString("Got handshake, connected!"), Logger::DBG);
                        ProtoJson::Msg msg = Msg::composeAck();
                        int wrote = socket()->write(Msg::serialise(msg));
                        Logger::log(QString("Sent Ack. Bytes=%1, contents=%2").arg(wrote).arg(Msg::serialise(msg).data()));
                        if (wrote == -1) {
                            Logger::log("Failed to write ack!!!", Logger::ERR);
                        }
                        socket()->write(Msg::serialise(msg));
                        Logger::log(QString("Sent ack: %1").arg(Msg::serialise(msg).constData()));
                        emit connStateChanged(connState, initiator, connType);
                    }
                    else {
                        Logger::log(QString("Expected handshake, got %1").arg(msg.type), Logger::ERR);
                    }
                    break;
                }
                case ConnState::AwaitingHandshakeReply: {
                    if (msg.type == MsgType::Ack) {
                        connState = ConnState::Connected;
                        initiator = true;
                        Logger::log(QString("Got hadshake reply, connected!"), Logger::DBG);
                        emit connStateChanged(connState, initiator, connType);
                    }
                    else {
                        Logger::log(QString("Expected handshake ack, got %1").arg(msg.type), Logger::ERR);
                    }
                    break;
                }
                case ConnState::Connected: {
                    //TODO - this check does not belong here
                    if (msg.type >= MsgType::Ack && msg.type <= MsgType::PlayMove) {
                        //forward the message
                        gameManager->onRemoteMessage(msg);
                    }
                    break;
                }
                default: {
                    Logger::log(QString("What is this state? Msg: %1").arg(msg.type), Logger::ERR);
                    Logger::log(QString("Unhandled message, type: %1, id: %2").arg(msg.type).arg(msg.msgid));
                    break;
                }
            }
        }
    }
}

bool ConnMan::activeConnection() const {
    return connState != ConnState::Disconnected;
}

/**
 * @brief ConnMan::sendMessage serialise a message to the peer
 */
void ConnMan::sendMessage(const ProtoJson::Msg& msg) {
    assert(socket());
    socket()->write(Msg::serialise(msg));
}

//TODO: should we maintain multiple buffers, one for each socket?
void ConnMan::dataAvailable() {
    QAbstractSocket* sock = qobject_cast<QAbstractSocket*>(sender());
    if ((void*)sock == (void*)btSocket) {
        buffer.append(btSocket->readAll());
    }
    else if (sock == tcpSocket) {
        buffer.append(tcpSocket->readAll());
    }
}

void ConnMan::socketDisconnected() {
    Logger::log(QString("%1 - deleting tcpSocket %2").arg(__func__).arg((int64_t)tcpSocket, 0, 16), Logger::ERR);
    tcpSocket->deleteLater();
    tcpSocket = nullptr;
}

void ConnMan::onSocketError() {
    if (tcpSocket) {
        Logger::log(QString("Socket error: %1").arg(tcpSocket->errorString()));
    }
}

void ConnMan::newConnectionTCP() {
    Logger::log(QString(LOG_POS));
    QTcpSocket* newSock = tcpServer->nextPendingConnection();

    if (newSock) {
        if (!tcpSocket) {
            tcpSocket = newSock;
            connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));
            connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(socketDisconnected()));
            Logger::log(QString("accepted new connection, local: %1, remote: %2").arg(tcpSocket->localPort()).arg(tcpSocket->peerPort()));
            initServerState();
        }
        else {
            QString message("A client is already connected. Refused connection becase we only support one client at a time!");
            Logger::log(QString("%1 - %2").arg(LOG_POS).arg(message));
            newSock->write(message.toUtf8());
            newSock->flush();
            const int timeout = 1000;
            newSock->disconnectFromHost();
            bool disconnected = ( (newSock->state() == QAbstractSocket::UnconnectedState) || newSock->waitForDisconnected(timeout));
            newSock->close();
            if (!disconnected) {
                Logger::log(QString("Could not disconnect socket in the allocated time. The ky may fall because I didn't implement proper cleanup."), Logger::ERR);
            }
            else {
                delete newSock;
            }
        }
    }
}

/**
 * @brief quick hack to avoid confusion between sockets.
 */
QIODevice *ConnMan::socket() {
    if (btSocket) {
        return btSocket;
    }
    else if (tcpSocket) {
        return tcpSocket;
    }
    return nullptr;
}

void ConnMan::initServerState() {
    connState = ConnState::AwaitingHandshakeReply;
    ProtoJson::Msg handshake = ProtoJson::Msg::composeHandshake();
    sendMessage(handshake);
}

void ConnMan::initClientState() {
    connState = ConnState::AwaitingHandshake;
}
