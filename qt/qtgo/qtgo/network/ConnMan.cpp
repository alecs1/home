#include "ConnMan.h"
#include "ProtoStructs.h"

using namespace ProtoBinary;

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
    msg.header.id = crtId;
    crtId += 1;
    msg.header.type = MsgType::Command;

    SCommand* command = new SCommand;
    command->type = CmdType::Connect;
    memcpy(command->uuid, uuid.toRfc4122().constData(), UUID_LEN);
    msg.command = command;
}

Peer* ConnMan::getPeer() {

}
