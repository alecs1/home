#include <QLayout>
#include <QToolButton>

#include "Settings.h"
#include "MiniGameControlWidget.h"

MiniGameControlWidget::MiniGameControlWidget(QWidget* parent) :
    QWidget(parent)
{
    delete layout();
    layoutP = new QVBoxLayout;
    setLayout(layoutP);
    passButton = new QToolButton(this);
    passButton->setText("Pass");
    passButton->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
    undoButton = new QToolButton(this);
    undoButton->setText("Undo");
    undoButton->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
    fullInterfaceButton = new QToolButton(this);
    fullInterfaceButton->setText("Show all\nbuttons");
    fullInterfaceButton->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
    layoutP->addStretch(2);
    layoutP->addWidget(passButton);
    layoutP->addStretch(2);
    layoutP->addWidget(undoButton);
    layoutP->addStretch(4);
    layoutP->addWidget(fullInterfaceButton);
    layoutP->setContentsMargins(0, 0, 0, 0);

    QObject::connect(passButton, SIGNAL(clicked(bool)), this, SIGNAL(userPassedMove()));
    QObject::connect(undoButton, SIGNAL(clicked(bool)), this, SIGNAL(undoMove()));
    QObject::connect(fullInterfaceButton, SIGNAL(clicked(bool)), this, SIGNAL(setFullInterface()));


    setAutoFillBackground(true);
    QPalette pal(palette());
    QColor backgCol(Settings::getProgramSettings()->tableColour);
    pal.setColor(QPalette::Background, backgCol);
    setPalette(pal);
}

void MiniGameControlWidget::addRoundInfo(RoundInfo* aRoundInfo) {
    roundInfo = aRoundInfo;
    layoutP->insertWidget(0, (QWidget*)roundInfo);
}

void MiniGameControlWidget::removeRoundInfo() {
    layoutP->removeWidget((QWidget*)roundInfo);
}

void MiniGameControlWidget::changeProgramSettings() {
    QPalette pal(palette());
    QColor backgCol(Settings::getProgramSettings()->tableColour);
    pal.setColor(QPalette::Background, backgCol);
    setPalette(pal);
    update();
}

void MiniGameControlWidget::setCurrentPlayer(int player, PlayerType type, PlayerType opponentType) {

    bool enableBlockingGroup = true;
    if (type == PlayerType::AI)
        enableBlockingGroup = false;

    passButton->setEnabled(enableBlockingGroup);
    undoButton->setEnabled(enableBlockingGroup);
}

