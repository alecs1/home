#ifndef ELLIPSE_H
#define ELLIPSE_H

#include "PhysicalObject.h"

class EllipseDefinition{
public:
	EllipseDefinition( float newW = 0, float newH = 0) {
		width = newW;
		height = newH;
	}
	//a qt-ish definition for the ellipse
	float width, height;
};

class Ellipse : public PhysicalObject {
public:
	Ellipse();
	void draw(QPainter& painter);
	void setHeight(float newH);
	void setWidth(float newW);
	virtual void move(Coordinate& newCoord);

protected:
	void testMarginColide();

protected:
	EllipseDefinition def;

private:
	QRectF rect;

private:
	void drawQt(QPainter& painter);
	void updateRect();

};

#endif
