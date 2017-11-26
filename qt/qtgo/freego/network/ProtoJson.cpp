#include "ProtoJson.h"

#include <QJsonDocument>

#include "../Logger.h"

namespace ProtoJson {

//const QString ProtoKw::MsgType = "MsgType";
//const QString ProtoKw::Request = "Request";
//const QString ProtoKw::Reply = "Reply";

/**
 * Parse the message enough to fill in the types. Expects a complete message!
 * @param data complete json message
 */
Msg Msg::parse(const QByteArray& data, int& lenParsed) {
    Logger::log(QString("will parse message: %1").arg(data.constData()));
    Msg msg;
    lenParsed = 0;

    char auxLen[LENGHT_LEN + 1] = {0};
    memcpy(auxLen, data.constData(), LENGHT_LEN);
    Logger::log(auxLen);
    bool validNum;
    int len = QString(auxLen).toInt(&validNum);

    if (!validNum) {
        return msg;
    }

    msg.json = QJsonDocument::fromJson(data.mid(HEADER_LEN, len)).object();
    msg.type = msgTypeMap.right.at(msg.json[ProtoKw::MsgType].toString());

    lenParsed = HEADER_LEN + len;

    //Logger::log(QString("Got valid message: %1").arg(data.left(lenParsed).constData()));
    return msg;
}

QByteArray Msg::serialise(const Msg &msg) {
    QJsonObject jsonObj;
    jsonObj[ProtoKw::MsgType] = msgTypeMap.left.at(msg.type);
    jsonObj[ProtoKw::Request] = msg.json;
    QByteArray data = QJsonDocument(jsonObj).toJson();
    int len = data.length();
    QString aux = QString("%1%2").arg(len, 9, 10, QChar('0')).arg('\n');
    //Logger::log("aux: " + aux);
    data.prepend(aux.toUtf8());
    return data;
}

Msg Msg::composeHandshake() {
    Msg msg;
    msg.type = MsgType::Hanshake;
    return msg;
}

Msg Msg::composeAck() {
    Msg msg;
    msg.type = MsgType::Ack;
    return msg;
}

bool Msg::msgValid(const Msg &msg) {
    return (msg.type != MsgType::MsgTypeCount);
}

} //namespace ProtoJson
