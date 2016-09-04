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

BTServer* ConnMan::getBTServer() const {
    return btServer;
}

void ConnMan::setBTClientSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connState = ConnState::AwaitingHandshakeReply;
    connect(btSocket, SIGNAL(readyRead()), this, SLOT(dataAvailable()));
}

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
                        Logger::log(QString("Got handshake, connected!"), LogLevel::DEBUG);
                        ProtoJson::Msg msg = Msg::composeAck();
                        btSocket->write(Msg::serialise(msg));
                    }
                    else {
                        Logger::log(QString("Expecting handshake, got %1").arg(msg.msgType), LogLevel::ERROR);
                    }
                    break;
                }
                case ConnState::AwaitingHandshakeReply: {
                    if (msg.msgType == MsgType::Ack) {
                        connState = ConnState::Connected;
                        initiator = true;
                        Logger::log(QString("Got hadshake reply, connected!"), LogLevel::DEBUG);
                    }
                    else {
                        Logger::log(QString("Expecting handshake ack, got %1").arg(msg.msgType), LogLevel::ERROR);
                    }
                    break;
                }
                default: {
                    break;
                }
            }
        }
    }
}

bool ConnMan::activeConnection() const {
    return connState != ConnState::Disconnected;
}

void ConnMan::dataAvailable() {
    buffer.append(btSocket->readAll());
}

