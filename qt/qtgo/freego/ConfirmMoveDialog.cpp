#include "Settings.h"

#include "ConfirmMoveDialog.h"
#include "ui_ConfirmMoveDialog.h"


ConfirmMoveDialog::ConfirmMoveDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ConfirmMoveDialog)
{
    ui->setupUi(this);
    connect(ui->confirmButton, SIGNAL(clicked()), this, SLOT(accept()));
    connect(ui->cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
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

/**
 * @brief Make the interface float over the table, like the menus do on mobile games. Currently disabled because it looks bad.
 */
void ConfirmMoveDialog::setMinimalInterface(bool aMinimal) {
    if (minimal == aMinimal) {
        return;
    }
    minimal = aMinimal;

    //TODO - his is currently inactive because it looks bad.
    /*
    if (minimal) {
        ui->verticalLayout->setSpacing(width() / 5);
        int space = width()/100;
        ui->verticalLayout->setContentsMargins(space, width() / 5, space, width() / 5);
        setAutoFillBackground(true);
        QPalette pal(palette());
        QColor backgCol(Settings::getProgramSettings()->tableColour);
        pal.setColor(QPalette::Background, backgCol);
        setPalette(pal);
    }
    */
}

void ConfirmMoveDialog::changeProgramSettings() {
    QPalette pal(palette());
    QColor backgCol(Settings::getProgramSettings()->tableColour);
    pal.setColor(QPalette::Background, backgCol);
    setPalette(pal);
    update();
}
