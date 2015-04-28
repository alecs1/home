#include <QPushButton>
#include <QLayout>
#include <QLabel>

#include "ThirdPartyInfo.h"

ThirdPartyInfo::ThirdPartyInfo (QWidget *parent) : QDialog(parent) {
    printf("%s\n", __func__);
    if (layout() != NULL)
        delete layout();

    //setTitle("Acknowledgements");

    QGridLayout* gridLayout= new QGridLayout();
    setLayout(gridLayout);

    okButton = new QPushButton("Close", this);
    gridLayout->addWidget(okButton, 1, 0);

    ackLabel = new QLabel(this);
    gridLayout->addWidget(ackLabel, 0, 0);
    populateAckLabel();

    connect(okButton,SIGNAL(clicked()), this, SLOT(accept()));
}

void ThirdPartyInfo::populateAckLabel() {
    QString labelContent =
            "<b>Sounds:</b></hr><br/>"
            "click.wav<br/>"
            "By lebcraftlp of freesound.org: http://www.freesound.org/people/lebcraftlp/sounds/192273/<br/>"
            "License: CC Attribution 3.0 Unported (http://creativecommons.org/licenses/by/3.0/legalcode)<br/>"
            "<br/>"
            "<b>Images:</b></hr><br/>"
            "playerAI.svg<br/"
            "playerHuman.svg<br/>"
            "playerNetwork.svg<br/>"
            "By the Oxygen Project: https://techbase.kde.org/Projects/Oxygen<br/>"
            "License: GNU LGPLv3 (http://www.gnu.org/licenses/lgpl-3.0.txt)<br/>"
            "<br/>"
            "Copies of the licenses have been included in the source code of this program in the licenses directory<br/>";

    ackLabel->setText(labelContent);
}
