#include "Rect.h"

#include <QtDebug>

Rect::Rect(float width, float height) {
	color.setRed(200);

	def.width = width;
	def.height = height;
}

void Rect::draw(QPainter& painter) {
	updateRect();
	drawQt(painter);
}

void Rect::drawQt(QPainter& painter) {
	painter.setBrush(color);
	painter.drawRect(rect);
}

void Rect::setWidth(float newW) {
	def.width = newW;
}

void Rect::setHeight(float newH) {
	def.height = newH;
}

void Rect::updateRect() {
	rect = QRectF(coord.x - def.width/2, coord.y - def.height/2, def.width, def.height);
}

void Rect::move(Coordinate& newCoord) {
	PhysicalObject::move(newCoord);
}

void Rect::testMarginColide() {

	float maxX = universeRect->width();
	float maxY = universeRect->height();
// 	qDebug() << "Rect::testMarginColide, width, height " << maxX << maxY << "for a universeRect: " << *universeRect;
	if(coord.x <= def.width / 2)
		speed.x = qAbs(speed.x);
	if(coord.y <= def.height / 2)
		speed.y = qAbs(speed.y);
	if(maxX - coord.x <= def.width / 2)
		speed.x = -qAbs(speed.x);
	if(maxY - coord.y <= def.height / 2)
		speed.y = -qAbs(speed.y);
}

float Rect::width() const {
	return def.width;
}

float Rect::height() const {
	return def.height;
}

GRect Rect::geometricalRect() const {
	return GRect (Coordinate(coord.x - def.width / 2, coord.y - def.height / 2), def.width, def.height);
}
