#ifndef PROTOJSON_H
#define PROTOJSON_H

#include <QByteArray>
#include <QJsonObject>
#include <stdint.h>

namespace ProtoJson {

struct ProtoJsonKW {
    QString MsgType = "MsgType";
    QString CmdType = "CmdType";
};

#define UUID_LEN 16

enum class MsgType:uint8_t {
    Command,
    Reply,
    MsgTypeCount
};

enum class CmdType:uint8_t {
    Hanshake,
    ListCommonGames,
    StartNewGame,
    ResumeGame,
    ResignGame,
    PlayMove,
    Disconnect,
    CmdTypeCount
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
    CmdType cType = CmdType::CmdTypeCount;
    char uuid[UUID_LEN];
    char isBlack;
    uint8_t row = 0xFF;
    uint8_t col = 0xFF;
    uint8_t size = 0xFF;
    float komi = -1000;
    uint8_t handicap = 0xFF;
    uint8_t handicapType = 0xFF;
};


QByteArray composeHandshake() {
    Msg msg;
    msg.msgType = MsgType::Command;
    msg.cmdType = CmdType::Hanshake;
    QJsonObject jsonObj;
    jsonObj[ProtoJsonKW::MsgType] = (uint8_t) msg.msgType;
    jsonObj[ProtoJsonKW::CmdType] = (uint8_t) msg.cmdType;
}


struct Msg {
    MsgType msgType = MsgType::MsgTypeCount;
    CmdType cmdType = CmdType::CmdTypeCount;
    unsigned int msgid = 0x0;
    QJsonObject json;
};

Msg parse(char* data);

}  //namespace ProtoJson

#endif // PROTOJSON_H
