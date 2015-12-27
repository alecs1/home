#include "ConnMan.h"

#include <QtNetwork/QAbstractSocket>

#include "ProtoJson.h"

using namespace ProtoJson;

const uint16_t tcpDefaultPort = 1491; //the first unasigned IANA port

ConnMan::ConnMan()
{
    uuid = QUuid::createUuid();
}

void ConnMan::connectTCP() {
    QAbstractSocket socket(QAbstractSocket::TcpSocket, this);
    bool bound = socket.bind(QHostAddress::Any, tcpDefaultPort);

    if (bound) {
        //we're the server, listen for others connecting
    }
    else {
        //someone already bound to on this computer, we may connect to them
    }
}

/*
 * @param msg - a message (Command or Reply), or NULL, in which case this is an update() call
 */
void ConnMan::processMessage(SMsg* msg) {

    if (connState == ConnState::Disconnected) {
        if (msg == NULL) {
            connect(getPeer());
        }
    }

}

void ConnMan::connect(Peer* peer) {
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
