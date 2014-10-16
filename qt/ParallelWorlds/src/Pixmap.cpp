#include "Pixmap.h"

#include <QtDebug>

Pixmap::Pixmap(QString fName) {
	loadImage(fName);
	respectRotation = false;
}

Pixmap::Pixmap(QImage image) {
	respectRotation = false;
	originalPixmap = QPixmap::fromImage(image);
	pixmap = originalPixmap;
	def.width = pixmap.width();
	def.height = pixmap.height();
}

void Pixmap::loadImage(QString fName) {
	fileName = fName;
	originalPixmap.load(fileName);
	pixmap = originalPixmap;
	def.width = pixmap.width();
	def.height = pixmap.height();
}

void Pixmap::draw(QPainter& painter) {
	updateRect();
	drawQt(painter);
}

void Pixmap::drawQt(QPainter& painter) {
	painter.setBrush(color);
	painter.drawPixmap(QPointF(coord.x - def.width / 2, coord.y - def.height / 2), pixmap);
}

void Pixmap::setWidth(float newW) {
	def.width = newW;
}

void Pixmap::setHeight(float newH) {
	def.height = newH;
}

void Pixmap::updateRect() {
	rect = QRectF(coord.x - def.width/2, coord.y - def.height/2, def.width, def.height);
}

void Pixmap::testMarginColide() {

	PhysicalObject::testMarginColide();

	float maxX = universeRect->width();
	float maxY = universeRect->height();
	//upper x margin:
	if(coord.x <= def.width / 2)
		speed.x = qAbs(speed.x);
	if(coord.y <= def.height / 2)
		speed.y = qAbs(speed.y);
	if(maxX - coord.x <= def.width / 2)
		speed.x = -qAbs(speed.x);
	if(maxY - coord.y <= def.height / 2)
		speed.y = -qAbs(speed.y);
}

void Pixmap::step(float& seconds) {
	if (respectRotation) {
// 		qDebug() << "sizes before: " << pixmap.width() << pixmap.height();
		QMatrix rotationMatrix;
		rotationMatrix.rotate(angleXY());
		pixmap = originalPixmap.transformed(rotationMatrix);
// 		qDebug() << "sizes after: " << pixmap.width() << pixmap.height();
		def.width = pixmap.width();
		def.height = pixmap.height();
	}
	else {
		//bla
	}

	PhysicalObject::step(seconds);
}

void Pixmap::setRespectRotation(bool respect) {
	respectRotation = respect;
}


