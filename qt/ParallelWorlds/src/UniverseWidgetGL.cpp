#include "UniverseWidgetGL.h"
#include "Universe.h"
#include "UniverseShowerGL.h"

#include <QTimer>
#include <QtDebug>
#include <QMouseEvent>
#include <QLayout>

UniverseWidgetGL::UniverseWidgetGL() {
// 	qDebug() << "UniverseWidgetGL";

	universe = NULL;

	GLChild = new UniverseShowerGL();
	QLayout* layout = this->layout();
	if (layout == NULL)
		layout = new QVBoxLayout;
	setLayout(layout);
	layout->addWidget(GLChild);

// 	qDebug() << layout->spacing();
// 	layout->setSpacing(0);
	layout->setContentsMargins(0, 1, 0, 0);
	int a, b, c, d;
	layout->getContentsMargins(&a, &b, &c, &d);
	qDebug() << a << b << c << d;

// 	GLChild->initializeGL();

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
// 	initializeGL();
}

void UniverseWidgetGL::paintEvent(QPaintEvent* event) {
	//QWidget part
// 	qDebug() << "UniverseWidgetGL::paintEvent()";
	QPainter painter(this);
	GLChild->setUniverse(universe);
	GLChild->update();
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

}

void UniverseWidgetGL::drawSettingsChanged(DrawSettings* newSettings) {
	GLChild->settingsChanged(newSettings);
}

// void UniverseWidgetGL::resizeEvent(QResizeEvent* event) {
// // 	qDebug() << "UniverseWidgetGL::resizeEvent()";
// 	palette.setBrush(this->backgroundRole(), QBrush(pixmap->scaled(event->size() ) ) );
// 	setPalette(palette);
// 	rect = this->contentsRect();
// 	if (universe != NULL)
// 		universe->setRect(&rect);
//
// }

// void UniverseWidgetGL::initializeGL() {
//  	qDebug("initializeGL()");
// 	QColor green(150, 200, 150);
// 	qglClearColor(green);
// 	glEnable(GL_DEPTH_TEST);
// }

// void UniverseWidgetGL::paintGL() {
// 	qDebug("paintGL");
//
// }

// void UniverseWidgetGL::resizeGL(int width, int height) {
// // 	qDebug("resizeGL()");
// 	glMatrixMode(GL_PROJECTION);
// }

//end

