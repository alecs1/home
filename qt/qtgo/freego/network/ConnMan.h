#pragma once

#include <QUuid>
#include <QObject>

class BTServer;
class QTcpServer;
class QBluetoothSocket;
class QTcpSocket;

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
    void connectBT(const QString address);
    uint16_t listenTCP();
    bool listenTCP(const QString address, const int port);
    void connectTCP(const QString address = "", const uint16_t port = 0);

    //BlueTooth related stuff
    BTServer* getBTServer() const;
    //void initBluetooth(const int interface);
    void setBTClientSocket(QBluetoothSocket* sock);
    void setBTServerSocket(QBluetoothSocket* sock);

    ProtoJson::Msg getMessage(int& parsedBytes);
    void processMessages();
    bool activeConnection() const;
    void sendMessage(const ProtoJson::Msg &msg);

public:
    //TODO - this is public for now
    ConnState connState = ConnState::Disconnected;
    bool initiator = false;

public slots:
    void dataAvailable();

signals:
    void connStateChanged(ConnMan::ConnState state, bool initiator, ConnMan::ConnType connType);

private:

    //ProtoState protoState = ProtoState::Idle;
    ConnType connType = ConnType::None;

    BTServer* btServer = nullptr;
    QBluetoothSocket* btSocket = nullptr;

    QTcpSocket* tcpSocket = nullptr;
    QTcpServer* tcpServer = nullptr;

    MainWindow* gameManager = nullptr;

    QByteArray buffer;
};
