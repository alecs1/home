#include "PeerWidget.h"
#include "network/ui_PeerWidget.h"

PeerWidget::PeerWidget(ConnType connType, const QString& name, const QString& address, QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PeerWidget)
{
    ui->setupUi(this);

    ui->peerName->setText(name);
    ui->peerAddr->setText(address);
    ui->connTypeImg->resize(ui->connTypeImg->height(), ui->connTypeImg->height());
}

void PeerWidget::setStrength(const int strength) {
    ui->connStrength->setText(QString::number(strength));
}

PeerWidget::~PeerWidget() {
    delete ui;
}

QString PeerWidget::address() const {
    return ui->peerAddr->text();
}
