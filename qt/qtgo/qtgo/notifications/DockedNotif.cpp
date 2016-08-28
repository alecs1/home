#include "DockedNotif.h"
#include "ui_DockedNotif.h"

namespace notifications {

DockedNotif::DockedNotif(const QString& text, const QList<Option> buttons, QWidget *parent) :
    QWidget(parent),
    ui(new Ui::DockedNotif)
{
    ui->setupUi(this);
}

DockedNotif::~DockedNotif()
{
    delete ui;
}

} //namespace notifications
