#include "ProtoJson.h"

#include <QJsonDocument>

#include "../Logger.h"

namespace ProtoJson {

const QString ProtoKw::MsgType = "MsgType";

/**
 * Parse the message enough to fill in the types. Expects a complete message!
 * @param data complete json message
 */
Msg Msg::parse(const QByteArray& data, int& lenParsed) {
    Msg msg;
    lenParsed = 0;

    char auxLen[LENGHT_LEN];
    memcpy(auxLen, data.constData(), LENGHT_LEN);
    bool validNum;
    int len = QString(auxLen).toInt(&validNum);

    if (!validNum) {
        return msg;
    }

    msg.json = QJsonDocument::fromJson(data.mid(HEADER_LEN, len)).object();

    lenParsed = HEADER_LEN + len;

    Logger::Log(QString("Got valid message: %1").arg(data.left(lenParsed).constData()));

    //bla, bagă mare la parsare
    return msg;
}

QByteArray Msg::serialise(const Msg &msg) {
    QJsonObject jsonObj;
    jsonObj[ProtoKw::MsgType] = (uint8_t) msg.msgType;
    QByteArray data = QJsonDocument(jsonObj).toJson();
    int len = data.length();
    QString aux = QString().arg(len, 9).arg('\n');
    data.prepend(aux.toUtf8());
    return data;
}

Msg Msg::composeHandshake() {
    Msg msg;
    msg.msgType = MsgType::Hanshake;
    return msg;
}

bool Msg::msgValid(const Msg &msg) {
    return (msg.msgType != MsgType::MsgTypeCount);
}

} //namespace ProtoJson
