#ifndef PROTOJSON_H
#define PROTOJSON_H

#include <QByteArray>
#include <QJsonObject>
#include <stdint.h>

namespace ProtoJson {

struct ProtoKw {
    static const QString MsgType;
    static const QString CmdType;
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


struct Msg {
    MsgType msgType = MsgType::MsgTypeCount;
    CmdType cmdType = CmdType::CmdTypeCount;
    unsigned int msgid = 0x0;
    QJsonObject json;
    static Msg parse(const char* data);
    static QByteArray serialise(const Msg& msg);
    static bool msgValid(const Msg& msg);
    static Msg composeHandshake();

private:
    static const int LENGHT_LEN = 9;
public:
    static const int HEADER_LEN = LENGHT_LEN + 1;

};


}  //namespace ProtoJson

#endif // PROTOJSON_H
