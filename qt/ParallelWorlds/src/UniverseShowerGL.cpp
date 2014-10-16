#include "UniverseShowerGL.h"
#include "Universe.h"

#include <QtDebug>
#include <QPainter>
#include <QResizeEvent>


UniverseShowerGL::UniverseShowerGL() {
	initializeGL();
	setAutoFillBackground(false);
	cleanBg = true;

	pixmap = new QPixmap("images/MCWorld/neutral-small.png");
	setMinimumSize(pixmap->size().width() / 1.4, pixmap->size().height() / 1.4);

	universe = NULL;
}

void UniverseShowerGL::setUniverse(Universe* aUniverse) {
	universe = aUniverse;
	universe->setWidget(this);
}

// void UniverseShowerGL::paintStart() {
// 	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
// 	QColor green(150, 200, 150); //tolerable greenish until I manage to put the picture on a wall
// 	qglClearColor(green);
// }

void UniverseShowerGL::initializeGL() {
//  	qDebug("initializeGL()");
	QColor green(150, 200, 150);
	qglClearColor(green);
	glEnable(GL_DEPTH_TEST);
}

void UniverseShowerGL::paintEvent(QPaintEvent* event) {
// 	qDebug() << "UniverseShowerGL::paintEvent()";
	if (cleanBg)
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	QColor green(150, 200, 150);
	qglClearColor(green);
	glEnable(GL_DEPTH_TEST);
	QPainter painter(this);
	if (universe != NULL)
		universe->draw(painter);
}

void UniverseShowerGL::resizeEvent(QResizeEvent* event) {
// 	qDebug() << "UniverseShowerGL::resizeEvent())";
	palette.setBrush(this->backgroundRole(), QBrush(pixmap->scaled(event->size() ) ) );
	setPalette(palette);
	rect = this->contentsRect();
	qDebug() << rect;
	if (universe != NULL)
		universe->setRect(&rect);

}

void UniverseShowerGL::settingsChanged(DrawSettings* newSettings) {
	if (newSettings->autoFillBg)
		setAutoFillBackground(true);
	else
		setAutoFillBackground(false);

	cleanBg = newSettings->autoCleanBg;
}

//end
