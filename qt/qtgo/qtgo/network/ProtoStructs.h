#ifndef PROTOSTRUCTS_H
#define PROTOSTRUCTS_H

#include <stdint.h>

//the low level, byte protocol
namespace Proto {

#define MAX_MSG_LEN 0xFFFF
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

struct SMsgHeader {
    uint16_t len = 0;
    MsgType type = MsgType::Invalid;
    uint16_t id = 0xFFFF;
    static unsigned int structSize();
};

struct SCommand {
    //SCommand();
    CmdType type = CmdType::Invalid;
    char partnerId[UUID_LEN];
    char isBlack;
    uint8_t row = 0xFF;
    uint8_t col = 0xFF;
    uint8_t size = 0xFF;
    uint8_t komiSign = 0xFF; //avoid signedness issues
    uint16_t komi = 0xFFFF; //NOTE: komi float number multiplied by 10
    uint8_t handicap = 0xFF;
    uint8_t handicapType = 0xFF;
};

struct SReply {
    ReplyType result = ReplyType::Invalid;
};

struct SMsg {
    SMsgHeader header;
    SCommand* command;
    SReply* reply;
};

int processMessage(char* buffer, unsigned int len);

SCommand parseCommand(SMsgHeader header, char* buffer, unsigned int len);
int serialiseCommand(SCommand cmd, char* buffer, unsigned int bufferLen);

} //namespace Proto

#endif // PROTOSTRUCTS_H

