#include "UniverseWidgetBase.h"
#include "Universe.h"

#include <QTimer>
#include <QtDebug>
#include <QMouseEvent>

void UniverseWidgetBase::setListerInterface(ListerInterface* newLister) {
	if (universe != NULL)
		universe->setListerInterface(newLister);
}

void UniverseWidgetBase::setUniverse(Universe* aUniverse) {
	qDebug() << "UniverseWidgetBase::setUniverse()";
	universe = aUniverse;
	universe->setWidget(this);
	qDebug() << "UniverseWidgetBase::setUniverse() - end";
}

void UniverseWidgetBase::paintEvent(QPaintEvent* event) {
	QPainter painter(this);
	if (universe != NULL)
		universe->draw(painter);
	QString aux;
	if(fixedFpsMode == false)
		aux = "Infinity";
	else
		aux.setNum(fps);
	QString wantedFpsText;
	wantedFpsText = "Fps wanted: " + aux;
	painter.drawText(QPointF(20, 20), wantedFpsText);
}

void UniverseWidgetBase::createConnections() {
	//actually disconnect the signal when universe is unset, more efficient than testing every time
	connect(universeTimer, SIGNAL(timeout()), this, SLOT(step()));
}

void UniverseWidgetBase::step() {
	if (universe != NULL)
		universe->step();
}

void UniverseWidgetBase::toggleFrameMode() {
	if(fixedFpsMode == true) {
		fixedFpsMode = false;
		universeTimer->setInterval(0);
	}
	else {
		fixedFpsMode = true;
		universeTimer->setInterval(1000*seconds);
	}
}

void UniverseWidgetBase::drawSettingsChanged(DrawSettings* newSettings) {
	//some class would better implement this
	qDebug() << "UniverseWidgetBase::drawSettingsChanged(), not implemented";
}

void UniverseWidgetBase::mouseReleaseEvent(QMouseEvent* event) {
	if(event->button() == Qt::MidButton) {
		//this would make the program try to get as many fps as posible, reenabled
		toggleFrameMode();
	}
	else if(event->button() == Qt::RightButton) {
		event->ignore();
	}
}

void UniverseWidgetBase::wheelEvent(QWheelEvent* event) {
	//delta returns the rotation of the wheel in 1/8 of a degree
	//typical mices work with steps of 15 degrees
	int numDegrees = event->delta() / 8;
	int numSteps = numDegrees / 15;

	fps += numSteps;
	seconds = 1.0 / fps;
	if(fixedFpsMode == false)
		toggleFrameMode();
 	universeTimer->setInterval(seconds * 1000);

}

void UniverseWidgetBase::resizeEvent(QResizeEvent* event) {
	qDebug() << "UniverseWidgetBase::resizeEvent()";
	palette.setBrush(this->backgroundRole(), QBrush(pixmap->scaled(event->size() ) ) );
	setPalette(palette);
	rect = this->contentsRect();
	if (universe != NULL)
		universe->setRect(&rect);
}
