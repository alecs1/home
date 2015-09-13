#ifndef CONNMAN_H
#define CONNMAN_H

#include <QUuid>


/*
namespace ProtoBinary {
    struct SMsg;
}
*/

namespace ProtoJson {
    struct SMsg;
}

struct Peer {

};

class ConnMan
{
private:
    enum class ConnState:uint8_t {
        Disconnected = 0,
        PendingConnection,
        Connected,
        InGame,
        Invalid = 0xFF
    };

    ConnState connState = ConnState::Disconnected;
    int crtId = 0;
    QUuid uuid;

private:

    void connect(Peer* peer);
    Peer* getPeer();
public:
    ConnMan();
    void processMessage(ProtoJson::SMsg* msg);
};

#endif // CONNMAN_H
