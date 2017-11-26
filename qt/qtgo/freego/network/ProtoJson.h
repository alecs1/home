#pragma once

#include <QByteArray>
#include <QJsonObject>
#include <stdint.h>

#include <boost/bimap.hpp>
#include <boost/assign.hpp>

namespace ProtoJson {

#define UUID_LEN 16

namespace ProtoKw {
    const QString MsgType = "MsgType";
    const QString Request = "Request";
    const QString Reply = "Reply";

    //MsgType
    const QString Ack = "Ack";
    const QString Success = "Success";
    const QString Fail = "Fail";
    const QString Error = "Error";
    const QString Disconnect = "Disconnect";
    const QString Hanshake= "Handshake";
    const QString ListCommonGames = "ListCommonGames";
    const QString StartNewGame = "StartNewGame";
    const QString ResumeGame = "ResumeGame";
    const QString ResignGame = "ResignGame";
    const QString PlayMove = "PlayMove";
}

//TODO - split connectivity from game messages that are reserved for the game.
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
    ListCommonGames,
    StartNewGame,
    ResumeGame,
    ResignGame,
    PlayMove,
    //Guard
    MsgTypeCount
};

const boost::bimap<MsgType, QString> msgTypeMap = boost::assign::list_of<boost::bimap<MsgType, QString>::relation>
        (MsgType::Ack, ProtoKw::Ack)
        (MsgType::Success, ProtoKw::Success)
        (MsgType::Fail, ProtoKw::Fail)
        (MsgType::Error, ProtoKw::Error)
        (MsgType::Disconnect, ProtoKw::Disconnect)
        (MsgType::Hanshake, ProtoKw::Hanshake)
        (MsgType::ListCommonGames, ProtoKw::ListCommonGames)
        (MsgType::StartNewGame, ProtoKw::StartNewGame)
        (MsgType::ResumeGame, ProtoKw::ResumeGame)
        (MsgType::ResignGame, ProtoKw::ResignGame)
        (MsgType::PlayMove, ProtoKw::PlayMove);

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
