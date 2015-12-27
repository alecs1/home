#ifndef CONNMAN_H
#define CONNMAN_H

#include <QUuid>
#include <QObject>


class QTcpServer;

namespace ProtoJson {
    struct SMsg;
}

struct Peer {

};

class ConnMan : public QObject
{
Q_OBJECT
private:
    enum ConnState:uint8_t {
        Disconnected = 0,
        Connected,
        InGame,
        Invalid = 0xFF
    };

    ConnState connState = ConnState::Disconnected;
    int crtId = 0;
    QUuid uuid;

    QTcpServer* tcpServer = NULL;

private:

    void connectToPeer(Peer* peer);
    Peer* getPeer();
    void newTcpConnection();

public:
    ConnMan();
    void connectTCP();
    void processMessage(ProtoJson::SMsg* msg);
};

#endif // CONNMAN_H
