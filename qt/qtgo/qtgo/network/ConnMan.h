#ifndef CONNMAN_H
#define CONNMAN_H

#include <QUuid>
#include <QObject>

class QBluetoothSocket;
class QAbstractSocket;

namespace ProtoJson {
    struct Msg;
}


class ConnMan : public QObject {
Q_OBJECT
public:
    enum ConnState:uint8_t {
        Disconnected = 0,
        SocketConnectedClient, //this instance initiated connection
        AwaitingHandshakeReply,
        SocketConnectedServer, //this instance accepted the connection
        AwaitingHandshake,
        Connected,
    };
    enum ProtoState:uint8_t {
        Idle,
        AwaitingReply,
        AwaitingMove,
    };
    enum ConnType:uint8_t {
        TCP,
        BT,
        None
    };

    ConnMan();
    ~ConnMan();
    void setBTClientSocket(QBluetoothSocket* sock);
    void setBTServerSocket(QBluetoothSocket* sock);
    ProtoJson::Msg getMessage(int& len);

public slots:
    void dataAvailable();

private:

    ConnState connState = ConnState::Disconnected;
    ProtoState protoState = ProtoState::Idle;
    ConnType connType = ConnType::None;

    QBluetoothSocket* btSocket = nullptr;
    QAbstractSocket* socket = nullptr;

    QByteArray buffer;
};












//Old prototypes, most likely there's nothing reusable
class QTcpServer;
class Peer;
class ConnManOld : public QObject
{
Q_OBJECT
private:
    enum ConnState:uint8_t {
        Disconnected = 0,
        SocketConnectedClient,
        SocketConnectedServer,
        AwaitingHandshakeReply,
        AwaitingHandshake,
        Connected,
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
    ConnManOld();
    void connectTCP();
    void processMessage(ProtoJson::Msg* msg);
};

#endif // CONNMAN_H
