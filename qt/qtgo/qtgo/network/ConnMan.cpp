#include "ConnMan.h"

#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QTcpServer>

#include "ProtoJson.h"

using namespace ProtoJson;

const uint16_t tcpDefaultPort = 1491; //the first unasigned IANA port

ConnMan::ConnMan()
{
    uuid = QUuid::createUuid();
}

void ConnMan::connectTCP() {
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

void ConnMan::newTcpConnection() {
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
void ConnMan::processMessage(SMsg* msg) {

    if (connState == ConnState::Disconnected) {
        if (msg == NULL) {
            connectToPeer(getPeer());
        }
    }

}

void ConnMan::connectToPeer(Peer* peer) {
    if (peer == NULL)
        return;

    SMsg msg;
    msg.mType = MsgType::Command;
    msg.msgid = crtId++;
    SCommand* cmd = new SCommand;
    msg.command = cmd;

    cmd->cType = CmdType::Connect;
    strncpy(cmd->uuid, uuid.toString().toUtf8().constData(), UUID_LEN);

}

Peer* ConnMan::getPeer() {

}
