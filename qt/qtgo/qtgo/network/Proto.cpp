#ifdef WIN32
#include <Winsock2.h>
#else
#include <arpa/inet.h>
#endif

#include <string.h>
#include <QtGlobal> //Q_ASSERT

#include "ProtoStructs.h"

namespace ProtoBinary {

unsigned int SMsgHeader::structSize() {
    return sizeof(SMsgHeader::len) + sizeof(SMsgHeader::type) + sizeof(SMsgHeader::id);
}

void someSafetyChecks() {
    static_assert(sizeof(SMsgHeader::len) == sizeof(uint16_t), "go change the ntohx htonx calls too!");
    static_assert(sizeof(SMsgHeader::type) == sizeof(uint8_t), "go change the ntohx htonx calls too!");
}

char* readStreamBytes(char* dest, char* source, unsigned int size) {
    memcpy(dest, source, size);
    return source + size;
}

char* writeStreamBytes(char* dest, char* source, unsigned int size) {
    memcpy(dest, source, size);
    return dest + size;
}

/*
 * @return - how many bytes have been processed; negative value: some error
 */
int readMessage(char* buffer, unsigned int len) {
    char* pos = buffer;
    if (len < sizeof(SMsgHeader::len)) {
        return 0;
    }

    SMsgHeader header;
    pos = readStreamBytes((char*)&header.len, pos, sizeof(SMsgHeader::len));
    header.len = ntohs(header.len);

    if (len < header.len) {
        return 0;
    }

    pos = readStreamBytes((char*)&header.type, pos, sizeof(SMsgHeader::type));

    pos = readStreamBytes((char*)&header.id, pos, sizeof(SMsgHeader::id));
    header.id = ntohs(header.id);

    int ret = -1;

    if (header.type == MsgType::Command) {
        SCommand cmd = parseCommand(header, pos, pos - buffer);
    }
    else {

    }

    return ret;
}

/*
struct SCommand {
    CmdType type;
    char partnerId[UUID_LEN];
    char isBlack;
    int row, col;
    int size;
    int komi;
    int handicap, handicapType;
};
*/

SCommand parseCommand(SMsgHeader header, char* buffer, unsigned int len) {
    SCommand cmd;
    char* pos = buffer;
    pos = readStreamBytes((char*)&cmd.type, pos, sizeof(SCommand::size));

    switch (cmd.type) {
    case CmdType::Connect:
        pos = readStreamBytes(cmd.uuid, pos, sizeof(SCommand::uuid));
        break;
    case CmdType::Disconnect:
        break;
    default:
        Q_ASSERT(false);
        break;
    }

    Q_ASSERT(pos - buffer == len);

    return cmd;
}

int serialiseCommand(SCommand cmd, char* buffer, unsigned int bufferLen) {
    char* pos = buffer;
    pos = writeStreamBytes(pos, (char*)&cmd.type, sizeof(SCommand::type));
    switch (cmd.type) {
    case CmdType::Connect:
        pos = writeStreamBytes(pos, cmd.uuid, sizeof(SCommand::uuid));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    return pos - buffer;
}

/*
 * @return = how many bytes were written; 0 and negative mean error
 */
int serialiseHeader(SMsgHeader header, char* buffer, unsigned int bufferLen) {
    char* pos = buffer;
    uint16_t len = htons(header.len);
    pos = writeStreamBytes(pos, (char*)&len, sizeof(header.len));
    pos = writeStreamBytes(pos, (char*)&header.type, sizeof(header.type));
    uint16_t id = htons(header.id);
    pos = writeStreamBytes(pos, (char*)&id, sizeof(header.id));
    Q_ASSERT(pos - buffer == SMsgHeader::structSize());
    return pos - buffer;
}

int serialiseMessage(SMsg msg, char* buffer, unsigned int bufferLen) {
    char* pos = buffer;
    //at this point we don't know the length
    pos += SMsgHeader::structSize();

    int contentSize = 0;
    switch (msg.header.type) {
    case MsgType::Command:
        contentSize = serialiseCommand(*msg.command, pos, bufferLen - SMsgHeader::structSize());
        break;
    case MsgType::Reply:
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    pos += contentSize;

    msg.header.len = contentSize + SMsgHeader::structSize();
    serialiseHeader(msg.header, buffer, SMsgHeader::structSize());

    Q_ASSERT(pos - buffer <= MAX_MSG_LEN);
    Q_ASSERT(pos - buffer == contentSize + SMsgHeader::structSize());
    return pos - buffer;
}



} //namespace Proto
