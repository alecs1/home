#pragma once

#include <QByteArray>
#include <QJsonObject>
#include <stdint.h>

namespace ProtoJson {

struct ProtoKw {
    static const QString MsgType;
    static const QString Request;
    static const QString Reply;
};

#define UUID_LEN 16


enum MsgType:uint8_t {
    //Replies:
    Ack,
    Success,
    Fail, //command understood but denied
    Error, //error in processing the command
    //Connection:
    Disconnect,
    Hanshake,
    //Game
    CommonGames,
    ListCommonGames,
    StartNewGame,
    ResumeGame,
    ResignGame,
    PlayMove,
    //Guard
    MsgTypeCount
};

struct Msg {
    MsgType type = MsgType::MsgTypeCount;
    unsigned int msgid = 0x0;
    QJsonObject json;

    static Msg parse(const QByteArray& data, int &lenParsed);
    static QByteArray serialise(const Msg& msg);
    static bool msgValid(const Msg& msg);
    static Msg composeHandshake();
    static Msg composeAck();

private:
    static const int LENGHT_LEN = 9;
public:
    static const int HEADER_LEN = LENGHT_LEN + 1;
};


} //namespace ProtoJson
