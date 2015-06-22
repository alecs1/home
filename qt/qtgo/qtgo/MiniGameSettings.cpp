#include <QLayout>
#include <QToolButton>

#include "Settings.h"
#include "MiniGameSettings.h"

MiniGameSettings::MiniGameSettings(QWidget* parent) :
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
    fullInterfaceButton->setText("Unhide\nbuttons");
    fullInterfaceButton->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
    layoutP->insertWidget(1, passButton);
    layoutP->addWidget(undoButton);
    layoutP->addStretch(5);
    layoutP->addWidget(fullInterfaceButton);
    layoutP->setSpacing(10);
    layoutP->setContentsMargins(0, 0, 0, 0);


    setAutoFillBackground(true);
    QPalette pal(palette());
    QColor backgCol(Settings::getProgramSettings()->tableColour);
    pal.setColor(QPalette::Background, backgCol);
    setPalette(pal);
}

void MiniGameSettings::addRoundInfo(RoundInfo* aRoundInfo) {
    roundInfo = aRoundInfo;
    layoutP->insertWidget(0, (QWidget*)roundInfo);
}

void MiniGameSettings::changeProgramSettings() {
    QPalette pal(palette());
    QColor backgCol(Settings::getProgramSettings()->tableColour);
    pal.setColor(QPalette::Background, backgCol);
    setPalette(pal);
    update();
}

