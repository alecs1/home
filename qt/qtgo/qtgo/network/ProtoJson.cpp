#include "ProtoJson.h"

#include <QJsonDocument>

namespace ProtoJson {

/**
 * Parse the message enough to fill in the types. Expects a complete message!
 * @param data complete json message
 */
void Msg::parse(char* data) {
    json = QJsonDocument::fromJson(data).object();
}

} //namespace ProtoJson
