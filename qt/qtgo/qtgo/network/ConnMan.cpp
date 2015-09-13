#include "ConnMan.h"
#include "ProtoJson.h"

using namespace ProtoJson;

ConnMan::ConnMan()
{
    uuid = QUuid::createUuid();
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
