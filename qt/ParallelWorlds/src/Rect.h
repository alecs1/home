#ifndef RECT_H
#define RECT_H

#include "PhysicalObject.h"
#include "geometry.h"

class RectDefinition{
public:
	RectDefinition( float newW = 0, float newH = 0) {
		width = newW;
		height = newH;
	}
	//a qt-ishdefinition for the ellipse
	float width, height;
};

//this shares almost all of the code with Ellipse
class Rect : public PhysicalObject {
public:
	Rect(float width, float height);
	void draw(QPainter& painter);
	void setHeight(float newH);
	void setWidth(float newW);
	float width() const;
	float height() const;
	virtual void move(Coordinate& newCoord);
	GRect geometricalRect() const;

protected:
	void testMarginColide();

protected:
	RectDefinition def;

private:
	QRectF rect;

private:
	void drawQt(QPainter& painter);
	void updateRect();

};

#endif
