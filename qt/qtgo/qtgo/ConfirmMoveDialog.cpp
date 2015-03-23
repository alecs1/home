#include "ConfirmMoveDialog.h"
#include "ui_ConfirmMoveDialog.h"

ConfirmMoveDialog::ConfirmMoveDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ConfirmMoveDialog)
{
    ui->setupUi(this);
    QObject::connect(ui->confirmButton, SIGNAL(clicked()), this, SLOT(accept()));
    QObject::connect(ui->cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
    icon = new QIcon;
}

ConfirmMoveDialog::~ConfirmMoveDialog()
{
    delete ui;
    delete icon;
}

void ConfirmMoveDialog::setPixmap(QPixmap& pixmap) {
    *icon = QIcon(pixmap);
    ui->confirmButton->setIcon(*icon);
    int diameter = this->size().width()/5;
    ui->confirmButton->setIconSize(QSize(diameter, diameter));
    //ui->confirmButton->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    //ui->confirmButton->setToolButtonStyle((Qt::ToolButtonTextBesideIcon));

    //TODO - now set a stylesheet.
    QString styleSheet =
            "QToolButton {\
                background-image: url(\":/resources/cursorBlack.svg\");\
                background-repeat: repeat-n;\
                background-position: center;\
                background-clip: padding;\
                border: 0 px;\
            }";
    Q_UNUSED(styleSheet);
    //ui->confirmButton->setStyleSheet(styleSheet);
}
