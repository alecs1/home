#include "ProtoJson.h"

#include <QJsonDocument>

namespace ProtoJson {

/**
 * Parse the message enough to fill in the types. Expects a complete message!
 * @param data complete json message
 */
Msg parse(char* data) {
    Msg msg;
    msg.json = QJsonDocument::fromJson(data).object();
    return msg;
}

} //namespace ProtoJson
