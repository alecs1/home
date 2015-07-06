#include <QMainWindow>
#include <QtBluetooth/QBluetoothServiceInfo>


class QPushButton;

class Main : public QMainWindow {
    Q_OBJECT
public:
    explicit Main(QWidget* parent = 0);
    ~Main();

private:
    QPushButton* button;

    QBluetoothServiceInfo serviceInfo;

public slots:
    int buttonClicked();
    int clientConnected();
};
