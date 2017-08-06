#pragma once

#include <QUuid>
#include <QObject>

class BTServer;
class QTcpServer;
class QBluetoothSocket;
class QTcpSocket;
class QIODevice;

class MainWindow; //temporary to forward the game related message. TODO: replace with a real class that only deals with this.

namespace ProtoJson {
    struct Msg;
}


class ConnMan : public QObject {
Q_OBJECT
public:
    enum ConnState:uint8_t {
        Disconnected = 0,
        SocketConnectedAsInitiator, //this instance initiated connection
        SocketConnectedAsHost, //this instance accepted the connection
        AwaitingHandshakeReply,
        AwaitingHandshake,
        Connected,
    };

    enum ConnType:uint8_t {
        TCP,
        BT,
        None
    };

    ConnMan(MainWindow* gameManager);
    ~ConnMan();

    //TCP
    uint16_t listenTCP();
    bool listenTCP(const QString address, const int port);
    bool connectTCP(const QString address = "", const uint16_t port = 0);

    //BlueTooth related stuff, needs heavy rework
    void connectBT(const QString address);
    BTServer* getBTServer() const;
    //void initBluetooth(const int interface);
    void setBTClientSocket(QBluetoothSocket* sock);
    void setBTServerSocket(QBluetoothSocket* sock);

    //Protocol and state
    ProtoJson::Msg getMessage(int& parsedBytes);
    void update();
    bool activeConnection() const;
    void sendMessage(const ProtoJson::Msg &msg);

public:
    //TODO - this is public for now
    ConnState connState = ConnState::Disconnected;
    bool initiator = false;

public slots:
    void dataAvailable();
    void socketDisconnected();
    void onSocketError();

signals:
    void connStateChanged(ConnMan::ConnState state, bool initiator, ConnMan::ConnType connType);

private slots:
    void newConnectionTCP();

private:

    QIODevice* socket();
    void initServerState();
    void initClientState();

    //ProtoState protoState = ProtoState::Idle;
    ConnType connType = ConnType::None;

    BTServer* btServer = nullptr;
    QBluetoothSocket* btSocket = nullptr;

    QTcpSocket* tcpSocket = nullptr;
    QTcpServer* tcpServer = nullptr;

    MainWindow* gameManager = nullptr;

    QByteArray buffer;
};
