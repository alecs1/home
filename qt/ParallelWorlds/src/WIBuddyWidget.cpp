#include "WIBuddyWidget.h"
#include "WorldBuddy.h"

#include <QHBoxLayout>
#include <QLabel>
#include <QToolButton>

WIBuddyWidget::WIBuddyWidget(WorldBuddy* nBuddy) {
	buddy = nBuddy;
	layout = new QHBoxLayout(this);
	setLayout(layout);

	nameLabel = new QLabel(buddy->getName(), this);
	layout->addWidget(nameLabel);

	showControlsButton = new QToolButton(this);
	showControlsButton->setText(tr("Controls"));
	layout->addWidget(showControlsButton);

	deleteButton = new QToolButton(this);
	deleteButton->setText(tr("Delete"));
	layout->addWidget(deleteButton);

	connect(deleteButton, SIGNAL(clicked()), this, SLOT(deleteWorld()));
// 	setMinimumSize(sizeHint());
}

QString WIBuddyWidget::name() const {
	return buddy->getName();
}

void WIBuddyWidget::deleteWorld() {
	buddy->deleteIt();
}