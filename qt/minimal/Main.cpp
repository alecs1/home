#include <QApplication>
#include <QPushButton>
#include <QLayout>
#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothServer>

#include "Main.h"

static const QLatin1String serviceUuid("e8e10f95-1a70-4b27-9ccf-02010264e9c8");


int main(int argc, char* argv[]) {
    printf("%s -started\n", __func__);

    QApplication app(argc, argv);

    Main m;
    m.show();
    
    return app.exec();
    qDebug() << __func__ << "-done";
}


Main::Main(QWidget* parent) : QMainWindow(parent)
{
    qDebug() << __func__ << "-started";
    button = new QPushButton("Click me", this);
    setCentralWidget(button);

    //if (layout() != NULL)
    //    delete layout();
    //QGridLayout *l = new QGridLayout(this);
    //setLayout(l);
    //l->addWidget(button, 0, 0);
    
    
    connect(button, SIGNAL(clicked()), this, SLOT(buttonClicked()));
    qDebug() << __func__ << "-done";
}

Main::~Main() {
    qDebug() << __func__ << "-done";
}

int Main::buttonClicked() {
    QList<QBluetoothHostInfo> localAdapters = QBluetoothLocalDevice::allDevices();
    printf("%s - localAdapters.count=%d\n", __func__, localAdapters.count());
    for(int i = 0; i < localAdapters.size(); i++) {
        qDebug() <<  __func__ << " - adapter: " <<  i <<
            " " << localAdapters[i].name().toUtf8().constData() <<
            " " << localAdapters[i].address().toString();
    }

    if (localAdapters.size() < 1) {
        qDebug() << __func__ << " no bluetooth found";
        return -1;
    }


    QBluetoothLocalDevice adapter(localAdapters.at(0).address());
    adapter.setHostMode(QBluetoothLocalDevice::HostDiscoverable);

    QBluetoothServer *rfcommServer = new QBluetoothServer(QBluetoothServiceInfo::RfcommProtocol, NULL);
    connect(rfcommServer, SIGNAL(newConnection()), this, SLOT(clientConnected()));

    QBluetoothAddress actualAddress = localAdapters[0].address();
    printf("%s - QBluetoothServer - starting to listen at %s\n", __func__,
           actualAddress.toString().toUtf8().constData());
    bool result = rfcommServer->listen(actualAddress);
    if (!result) {
        qDebug() <<  __func__ << "-" << result;
        return -1;
    }

    //! [Get local device name]
    QString localName = QBluetoothLocalDevice().name();
    qDebug() << "local device name" << localName;

    //result = serviceInfo.registerService(/*actualAddress*/);

#pragma region Attributes
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceRecordHandle, (uint)0x00010010);

    //! [Class Uuuid must contain at least 1 entry]
    QBluetoothServiceInfo::Sequence classId;

    classId << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::SerialPort));
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,
                             classId);

    printf("%s - done %d\n", __func__, 1);

    classId.prepend(QVariant::fromValue(QBluetoothUuid(serviceUuid)));

    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceClassIds, classId);
    serviceInfo.setAttribute(QBluetoothServiceInfo::BluetoothProfileDescriptorList,classId);
    //! [Class Uuuid must contain at least 1 entry]


    printf("%s - done %d\n", __func__, 2);
    //! [Service name, description and provider]
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceName, tr("Bt Chat Server"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceDescription,
                             tr("Example bluetooth chat server"));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ServiceProvider, tr("qt-project.org"));
    //! [Service name, description and provider]

    //! [Service UUID set]
    serviceInfo.setServiceUuid(QBluetoothUuid(serviceUuid));
    //! [Service UUID set]


    printf("%s - done %d\n", __func__, 3);
    //! [Service Discoverability]
    QBluetoothServiceInfo::Sequence publicBrowse;
    publicBrowse << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::PublicBrowseGroup));
    serviceInfo.setAttribute(QBluetoothServiceInfo::BrowseGroupList,
                             publicBrowse);
    //! [Service Discoverability]

    printf("%s - done %d\n", __func__, 4);
    //! [Protocol descriptor list]
    QBluetoothServiceInfo::Sequence protocolDescriptorList;
    QBluetoothServiceInfo::Sequence protocol;
    protocol << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::L2cap));
    protocolDescriptorList.append(QVariant::fromValue(protocol));
    protocol.clear();
    protocol << QVariant::fromValue(QBluetoothUuid(QBluetoothUuid::Rfcomm))
             << QVariant::fromValue(quint8(rfcommServer->serverPort()));
    protocolDescriptorList.append(QVariant::fromValue(protocol));
    serviceInfo.setAttribute(QBluetoothServiceInfo::ProtocolDescriptorList,
                             protocolDescriptorList);
    //! [Protocol descriptor list]

    printf("%s - done %d\n", __func__, 5);
    //! [Register service]
    result = serviceInfo.registerService(actualAddress);
    if (!result) {
        printf("%s - registering with SDP failed\n", __func__);
        return -1;
    }
    //! [Register service]
#pragma endregion

    printf("%s - done\n", __func__);
    return 0;


}


int Main::clientConnected() {
    qDebug() << __func__;
    return 0;
}
