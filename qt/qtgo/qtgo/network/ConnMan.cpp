#include "ConnMan.h"

#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QTcpServer>

#include <QtBluetooth/QBluetoothSocket>
#include <arpa/inet.h>

#include "ProtoJson.h"

using namespace ProtoJson;

const uint16_t tcpDefaultPort = 1491; //the first unasigned IANA port

ConnMan::ConnMan() {

}

ConnMan::~ConnMan() {

}

void ConnMan::setBTClientSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connState = ConnState::AwaitingHandshakeReply;
}

void ConnMan::setBTServerSocket(QBluetoothSocket* sock) {
    btSocket = sock;
    connState = ConnState::AwaitingHandshake;
}

/**
 * Read the next message from buffer, if available
 * @param lenght of the message read
 * @return
 */
ProtoJson::Msg ConnMan::getMessage(int& len) {
    len = 0;
    if(buffer.size() >= 4) {
        uint32_t networkLen = 0;
        uint32_t msgLen = 0;
        memcpy(&networkLen, buffer.data(), 4);
        msgLen = ntohl(networkLen);

        if (buffer.size() >= msgLen) {
            ProtoJson::Msg ret;
            ret.parse(buffer.data() + 4);
        }
    }
}

void ConnMan::dataAvailable() {
    buffer.append(btSocket->readAll());
}














ConnManOld::ConnManOld()
{
    uuid = QUuid::createUuid();
}

void ConnManOld::connectTCP() {
    //QAbstractSocket socket(QAbstractSocket::TcpSocket, this);
    //bool bound = socket.QHostAddress::Any, tcpDefaultPort);

    if (tcpServer == NULL) {
        tcpServer = new QTcpServer(this);
    }
    bool bound = tcpServer->listen(QHostAddress::Any, tcpDefaultPort);

    if (bound) {
        //we're the server, listen for others connecting
        connect(tcpServer, SIGNAL(newConnection()), this, SLOT(newTcpConnection()));
    }
    else {
        //someone already bound to on this computer, we may connect to them
    }
}

void ConnManOld::newTcpConnection() {
    QTcpSocket* socket = tcpServer->nextPendingConnection();
    if(connState == ConnState::Disconnected) {
        printf("%s - accepted new connection\n", __func__);
    }
    else {
        printf("%s - will reject new connection since we're already connected\n", __func__);
        QString msg =
                "{\
                      \"msg_type\":\"command\",\
                      \"command\":{\
                          \"type\":\"disconnect\",\
                          \"reason\":\"already_connected\"\
                      }\
                 }";
        socket->write(msg.toUtf8());
        socket->disconnectFromHost();
    }
}

/*
 * @param msg - a message (Command or Reply), or NULL, in which case this is an update() call
 */
void ConnManOld::processMessage(Msg* msg) {

    if (connState == ConnState::Disconnected) {
        if (msg == NULL) {
            connectToPeer(getPeer());
        }
    }

}

void ConnManOld::connectToPeer(Peer* peer) {
    if (peer == NULL)
        return;

    Msg msg;
    msg.msgType = MsgType::Command;
    msg.msgid = crtId++;
    //SCommand* cmd = new SCommand;
    //msg.command = cmd;

    msg.cmdType = CmdType::Connect;
    //strncpy(cmd->uuid, uuid.toString().toUtf8().constData(), UUID_LEN);

}

Peer* ConnManOld::getPeer() {

}
