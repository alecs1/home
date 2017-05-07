#ifndef PEERWIDGET_H
#define PEERWIDGET_H

#include <QWidget>

enum ConnType : uint8_t {
    ConnBT,
    ConnIP,
    ConnServer,
};

namespace Ui {
class PeerWidget;
}

class PeerWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PeerWidget(ConnType connType, const QString& name, const QString& address, QWidget *parent = 0);
    void setStrength(const int strength);
    ~PeerWidget();
    QString address() const;

private:
    Ui::PeerWidget *ui;
};

#endif // PEERWIDGET_H
