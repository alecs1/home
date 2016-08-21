#include "ProtoJson.h"

#include <QJsonDocument>

namespace ProtoJson {

const QString ProtoKw::MsgType = "MsgType";
const QString ProtoKw::CmdType = "CmdType";

/**
 * Parse the message enough to fill in the types. Expects a complete message!
 * @param data complete json message
 */
Msg Msg::parse(const char* data) {
    Msg msg;
    char auxLen[LENGHT_LEN];
    memcpy(auxLen, data, LENGHT_LEN);
    bool validNum;
    int len = QString(auxLen).toInt(&validNum);

    if (!validNum) {
        return msg;
    }

    msg.json = QJsonDocument::fromJson(data + HEADER_LEN).object();

    //bla, bagă mare
    return msg;
}

QByteArray Msg::serialise(const Msg &msg) {
    QJsonObject jsonObj;
    jsonObj[ProtoKw::MsgType] = (uint8_t) msg.msgType;
    jsonObj[ProtoKw::CmdType] = (uint8_t) msg.cmdType;
    QByteArray data = QJsonDocument(jsonObj).toJson();
    int len = data.length();
    QString aux = QString().arg(len, 9).arg('\n');
    data.prepend(aux.toUtf8());
    return data;
}

Msg Msg::composeHandshake() {
    Msg msg;
    msg.msgType = MsgType::Command;
    msg.cmdType = CmdType::Hanshake;
}

bool Msg::msgValid(const Msg &msg) {
    return (msg.msgType != MsgType::MsgTypeCount && msg.cmdType != CmdType::CmdTypeCount);
}

} //namespace ProtoJson
