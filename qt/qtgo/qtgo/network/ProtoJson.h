#ifndef PROTOJSON_H
#define PROTOJSON_H

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

struct Msg {
    MsgType mType = MsgType::Invalid;
    uint msgid = 0x0;
};

struct Command {
    CmdType cType = CmdType::Invalid;
};


}

#endif // PROTOJSON_H
