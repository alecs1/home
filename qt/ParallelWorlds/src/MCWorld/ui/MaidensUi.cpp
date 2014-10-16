#include "MaidensUi.h"
#include <QFileDialog>


MaidensUi::MaidensUi(QWidget* parent) : QWidget(parent) {
	setupUi(this);
	connect (openButton, SIGNAL(clicked()), this, SLOT(openFile()));
	connect(backButton, SIGNAL(clicked()), this, SIGNAL(back()));
	connect(forwardButton, SIGNAL(clicked()), this, SIGNAL(forward()));
	connect(speedSpinBox, SIGNAL(valueChanged(double)), this, SIGNAL(speedChanged(double)));
	connect(startButton, SIGNAL(clicked()), this, SIGNAL(start()));
	connect(randomButton, SIGNAL(clicked()), this, SIGNAL(random()));
}

void MaidensUi::openFile() {
	QString fileName = QFileDialog::getOpenFileName(this, "Select a route file");
	if (fileName != "")
		emit fileChosen(fileName);
}
