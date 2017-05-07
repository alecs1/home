#include "ConnMan.h"

#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QTcpServer>
#include <QtBluetooth/QBluetoothSocket>

#include "BTServer.h"
#include "ProtoJson.h"
#include "../Logger.h"

using namespace ProtoJson;

const uint16_t tcpDefaultPort = 1491; //the first unasigned IANA port

ConnMan::ConnMan() {
    btServer = new BTServer(this);
}

ConnMan::~ConnMan() {

}

void ConnMan::connectBT(const QString address) {
    btServer->connectAddress(address);
    connState = ConnState::AwaitingHandshakeReply;
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
                    if (msg.msgType == MsgType::Hanshake) {
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
                        Logger::log(QString("Expecting handshake, got %1").arg(msg.msgType), LogLevel::ERR);
                    }
                    break;
                }
                case ConnState::AwaitingHandshakeReply: {
                    if (msg.msgType == MsgType::Ack) {
                        connState = ConnState::Connected;
                        initiator = true;
                        Logger::log(QString("Got hadshake reply, connected!"), LogLevel::DBG);
                        emit connStateChanged(connState, initiator, connType);
                    }
                    else {
                        Logger::log(QString("Expecting handshake ack, got %1").arg(msg.msgType), LogLevel::ERR);
                    }
                    break;
                }
                case ConnState::Connected: {
                    if (msg.msgType >= MsgType::CommonGames && msg.MsgType <= PlayMove) {
                        //forward the message
                    }
                }
                default: {
                    Logger::log(QString("What is this state? Msg: %1").arg(msg.msgType), LogLevel::ERR);
                    Logger::log(QString("Unhandled message, type: %1, id: %2").arg(msg.msgType).arg(msg.msgid));
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
void ConnMan::sendMessage(ProtoJson::Msg& msg) {
    btSocket->write(Msg::serialise(msg));
}

void ConnMan::dataAvailable() {
    buffer.append(btSocket->readAll());
}

