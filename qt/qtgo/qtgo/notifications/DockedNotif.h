#pragma once

#include <QWidget>

namespace Ui {
class DockedNotif;
}

namespace notifications {

enum OptionType:uint8_t {
    Accept,
    Cancel,
    TakeControl,
};

struct Option {
    int val;
    QString text;
    //icon?
};

//Note: this is non standard, works with gcc
const Option OPTION_ACCEPT = { .val = OptionType::Accept, .text = "Accept" };
const Option OPTION_CANCEL = { .val = OptionType::Cancel, .text = "Cancel" };
const Option OPTION_TAKE_CONTROL = { .val = OptionType::TakeControl, .text = "Edit Game" };

class DockedNotif : public QWidget
{
    Q_OBJECT

public:
    explicit DockedNotif(const QString& text, const QList<Option> buttons, QWidget *parent = 0);
    ~DockedNotif();

private:
    Ui::DockedNotif *ui;
};

} //namespace notifications
