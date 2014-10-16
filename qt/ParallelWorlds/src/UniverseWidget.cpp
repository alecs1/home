#include "UniverseWidget.h"
#include "Universe.h"

#include <QTimer>
#include <QtDebug>
#include <QMouseEvent>

UniverseWidget::UniverseWidget() {
// 	qDebug() << "UniverseWidget";

	universe = NULL;

	//make this changable from a world class
	pixmap = new QPixmap("images/MCWorld/neutral-small.png");
	setAutoFillBackground(true);

	fps = 25;
	fixedFpsMode = true;

	seconds = 1.0/ fps;
	universeTimer = new QTimer(this);
	universeTimer->start(1000 * seconds);

	//1,4 - a random value that fitted well on my screen
	setMinimumSize(pixmap->size().width() / 1.4, pixmap->size().height() / 1.4);

	createConnections();
}

void UniverseWidget::drawSettingsChanged(DrawSettings* newSettings) {
	if (newSettings->autoFillBg)
		setAutoFillBackground(true);
	else
		setAutoFillBackground(false);
}

// void UniverseWidget::setListerInterface(ListerInterface* newLister) {
// 	if (universe != NULL)
// 		universe->setListerInterface(newLister);
// }
//
// void UniverseWidget::setUniverse(Universe* aUniverse) {
// // 	qDebug() << "UniverseWidget::setUniverse()";
// // 	universe = aUniverse;
// // 	universe->setWidget(this);
// 	UniverseWidgetBase::setUniverse(aUniverse);
// }
//
// void UniverseWidget::paintEvent(QPaintEvent* event) {
// 	QPainter painter(this);
// 	if (universe != NULL)
// 		universe->draw(painter);
// 	QString aux;
// 	if(fixedFpsMode == false)
// 		aux = "Infinity";
// 	else
// 		aux.setNum(fps);
// 	QString wantedFpsText;
// 	wantedFpsText = "Fps wanted: " + aux;
// 	painter.drawText(QPointF(20, 20), wantedFpsText);
// }
//
// void UniverseWidget::createConnections() {
// 	//actually disconnect the signal when universe is unset, more efficient than testing every time
// 	connect(universeTimer, SIGNAL(timeout()), this, SLOT(step()));
// }
//
// void UniverseWidget::step() {
// 	if (universe != NULL)
// 		universe->step();
// }
//
// void UniverseWidget::toggleFrameMode() {
// 	if(fixedFpsMode == true) {
// 		fixedFpsMode = false;
// 		universeTimer->setInterval(0);
// 	}
// 	else {
// 		fixedFpsMode = true;
// 		universeTimer->setInterval(1000*seconds);
// 	}
// }
//
// void UniverseWidget::mouseReleaseEvent(QMouseEvent* event) {
// 	if(event->button() == Qt::MidButton) {
// 		//this would make the program try to get as many fps as posible, reenabled
// 		toggleFrameMode();
// 	}
// 	else if(event->button() == Qt::RightButton) {
// 		event->ignore();
// 	}
// }
//
// void UniverseWidget::wheelEvent(QWheelEvent* event) {
// 	//delta returns the rotation of the wheel in 1/8 of a degree
// 	//typical mices work with steps of 15 degrees
// 	int numDegrees = event->delta() / 8;
// 	int numSteps = numDegrees / 15;
//
// 	fps += numSteps;
// 	seconds = 1.0 / fps;
// 	if(fixedFpsMode == false)
// 		toggleFrameMode();
//  	universeTimer->setInterval(seconds * 1000);
//
// }
//
// void UniverseWidget::resizeEvent(QResizeEvent* event) {
// // 	qDebug() << "resizeEvent";
// 	palette.setBrush(this->backgroundRole(), QBrush(pixmap->scaled(event->size() ) ) );
// 	setPalette(palette);
// 	rect = this->contentsRect();
// 	if (universe != NULL)
// 		universe->setRect(&rect);
// }

//end
