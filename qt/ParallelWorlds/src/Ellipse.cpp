#include "Ellipse.h"

#include <QtDebug>

Ellipse::Ellipse() {
	color.setRed(200);

	def.width = 50;
	def.height = 50;
}

void Ellipse::draw(QPainter& painter) {
	updateRect();
	drawQt(painter);
}

void Ellipse::drawQt(QPainter& painter) {
	painter.setBrush(color);
	painter.drawEllipse(rect);
}

void Ellipse::setWidth(float newW) {
	def.width = newW;
}

void Ellipse::setHeight(float newH) {
	def.height = newH;
}

void Ellipse::updateRect() {
	rect = QRectF(coord.x - def.width/2, coord.y - def.height/2, def.width, def.height);
}

void Ellipse::move(Coordinate& newCoord) {
	PhysicalObject::move(newCoord);
}


void Ellipse::testMarginColide() {

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
