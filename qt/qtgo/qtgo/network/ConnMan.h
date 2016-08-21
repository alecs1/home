#pragma once

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
    ProtoJson::Msg getMessage(int& parsedBytes);
    void processMessages();
    bool activeConnection() const;

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
