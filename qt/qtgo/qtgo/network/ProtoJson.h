#ifndef PROTOJSON_H
#define PROTOJSON_H

#include <QByteArray>

#include <stdint.h>

namespace ProtoJson {

#define UUID_LEN 16

enum class MsgType:uint8_t {
    Command,
    Reply,
    Invalid = 0xFF
};

enum class CmdType:uint8_t {
    Connect,
    ListCommonGames,
    StartNewGame,
    ResumeGame,
    ResignGame,
    PlayMove,
    Disconnect,
    Invalid = 0xFF
};

enum class CommState:uint8_t {
    NotConnected,
    Connected,
    InGame,
    Invalid = 0xFF
};

enum class ReplyType:uint8_t {
    Success = 0,
    Fail, //command understood but denied
    Error, //error in processing the command
    Invalid = 0xFF
};

struct SReply {
    ReplyType result = ReplyType::Invalid;
};

struct SCommand {
    CmdType cType = CmdType::Invalid;
    char uuid[UUID_LEN];
    char isBlack;
    uint8_t row = 0xFF;
    uint8_t col = 0xFF;
    uint8_t size = 0xFF;
    float komi = -1000;
    uint8_t handicap = 0xFF;
    uint8_t handicapType = 0xFF;
};


struct SMsg {
    MsgType mType = MsgType::Invalid;
    unsigned int msgid = 0x0;
    SCommand* command = NULL;
    SReply* reply = NULL;
};

QByteArray serialiseReply(SReply* reply) {
}

QByteArray serialiseCmd(SCommand* cmd) {

}


}  //namespace ProtoJson

#endif // PROTOJSON_H
