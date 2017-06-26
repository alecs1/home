#include "ConnMan.h"

#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QTcpServer>
#include <QtBluetooth/QBluetoothSocket>

#include "BTServer.h"
#include "ProtoJson.h"
#include "../Logger.h"
#include "../mainwindow.h" //TODO - this is only temporary

using namespace ProtoJson;

//See for a way to choose these ports: https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers#Registered_ports
const uint16_t tcpDefaultFirstPort = 1030;
const uint16_t tcpDefaultLastPort = 1050;

ConnMan::ConnMan(MainWindow *gameManager) : gameManager(gameManager) {
    btServer = new BTServer(this);
    tcpServer = new QTcpServer(this);
    tcpSocket = new QTcpSocket(this);
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
 * Listen on a specific address and port
 */
bool ConnMan::listenTCP(const QString address, const int port) {
    if (tcpServer->isListening()) {
        Logger::log(QString("Will stop listening on port %1 and start listening on port %2").arg(tcpServer->serverPort()).arg(port));
    }
    QHostAddress addr(address);
    bool success = tcpServer->listen(addr, port);
    if (success) {
        Logger::log(QString("Listening on TCP port %1").arg(port));
    }
    else {
        Logger::log(QString("Failed to listen on TCP port %1").arg(port), Logger::ERR);
    }
    return success;
}

/**
 * Try to connect on TCP
 * @param address - if missing it will default to localhost
 * @param port - if missing default to the entire default range
 */
void ConnMan::connectTCP(const QString address/* = ""*/, const uint16_t port/* = 0*/) {
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

    bool success = false;
    uint16_t crtPort = firstPort;
    while (!success && crtPort <= lastPort) {

    }
}

BTServer* ConnMan::getBTServer() const {
    return btServer;
}

/**
 * @brief ConnMan::setBTClientSocket set the socket to a client connection, we're behaving as server
 */
void ConnMan::setBTClientSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connState = ConnState::AwaitingHandshakeReply;
    connect(btSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));

    ProtoJson::Msg handshake = ProtoJson::Msg::composeHandshake();
    QByteArray data = ProtoJson::Msg::serialise(handshake);
    btSocket->write(data);
    Logger::log(QString("%1 - wrote \"%2\" to socket.").arg(__PRETTY_FUNCTION__).arg(data.constData()), LogLevel::DBG);
}

/**
 * @brief ConnMan::setBTServerSocket set the socket to a server connection, we're behaving as a client */
void ConnMan::setBTServerSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connState = ConnState::AwaitingHandshake;
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

void ConnMan::processMessages() {
    if (buffer.size() > 0) {
        int len = 0;
        ProtoJson::Msg msg = getMessage(len);
        if (len > 0) {
            switch (connState) {
                case ConnState::AwaitingHandshake: {
                    if (msg.type == MsgType::Hanshake) {
                        connState = ConnState::Connected;
                        Logger::log(QString("Got handshake, connected!"), LogLevel::DBG);
                        ProtoJson::Msg msg = Msg::composeAck();
                        int wrote = btSocket->write(Msg::serialise(msg));
                        Logger::log(QString("Sent Ack. Bytes=%1, contents=%2").arg(wrote).arg(Msg::serialise(msg).data()));
                        if (wrote == -1) {
                            Logger::log("Failed to write ack!!!", LogLevel::ERR);
                        }
                        btSocket->write(Msg::serialise(msg));
                        Logger::log(QString("Sent ack: %1").arg(Msg::serialise(msg).constData()));
                        emit connStateChanged(connState, initiator, connType);
                    }
                    else {
                        Logger::log(QString("Expecting handshake, got %1").arg(msg.type), LogLevel::ERR);
                    }
                    break;
                }
                case ConnState::AwaitingHandshakeReply: {
                    if (msg.type == MsgType::Ack) {
                        connState = ConnState::Connected;
                        initiator = true;
                        Logger::log(QString("Got hadshake reply, connected!"), LogLevel::DBG);
                        emit connStateChanged(connState, initiator, connType);
                    }
                    else {
                        Logger::log(QString("Expecting handshake ack, got %1").arg(msg.type), LogLevel::ERR);
                    }
                    break;
                }
                case ConnState::Connected: {
                    if (msg.type >= MsgType::Ack && msg.type <= MsgType::PlayMove) {
                        //forward the message
                        gameManager->onRemoteMessage(msg);
                    }
                    break;
                }
                default: {
                    Logger::log(QString("What is this state? Msg: %1").arg(msg.type), LogLevel::ERR);
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
    btSocket->write(Msg::serialise(msg));
}

void ConnMan::dataAvailable() {
    buffer.append(btSocket->readAll());
}

