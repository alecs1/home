#include "WorldEmisarWidget.h"
#include "WorldEmisar.h"

#include <QHBoxLayout>
#include <QLabel>
#include <QToolButton>

WorldEmisarWidget::WorldEmisarWidget(WorldEmisar* nEmisar) {
	emisar = nEmisar;
	layout = new QHBoxLayout(this);
	setLayout(layout);

	nameLabel = new QLabel(emisar->getName(), this);
	nameLabel->setToolTip(emisar->getDescription());
	layout->addWidget(nameLabel);

	createButton = new QToolButton(/*tr("Create"), */this);
	createButton->setText(tr("Create"));
	createButton->setToolTip(tr("Create a new ") + emisar->getName());
	layout->addWidget(createButton);

	connect(createButton, SIGNAL(clicked()), this, SLOT(makeInstance()));
}

void WorldEmisarWidget::makeInstance() {
	emisar->makeInstance();
}


QString WorldEmisarWidget::name() const {
	return emisar->getName();
}
