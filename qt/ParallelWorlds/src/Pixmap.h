#ifndef PIXMAP_H
#define PIXMAP_H

#include "PhysicalObject.h"

class PixmapDefinition{
public:
	PixmapDefinition( float newW = 0, float newH = 0) {
		width = newW;
		height = newH;
	}
	virtual ~PixmapDefinition() {} //if this is not virtual the program will not exit cleanly !!!
	float width, height;
};

class Pixmap : public PhysicalObject {
public:
	Pixmap(QString fName = "images/icons/parallel worlds.png");
	Pixmap(QImage image);
	void loadImage(QString fName = "images/icons/parallel worlds.png");
	void draw(QPainter& painter);
	void setHeight(float newH);
	void setWidth(float newW);
	//must reimplement a part of it
	void step(float& seconds);
	void setRespectRotation(bool respect);

protected:
	void testMarginColide();
	//candidate to move in an upper class


protected:
	PixmapDefinition def;

private:
	QRectF rect;
	QPixmap originalPixmap;
	QPixmap pixmap;
	QString fileName;
	//many things have to change once the rotation is made
	bool respectRotation;

private:
	void drawQt(QPainter& painter);
	void updateRect();
};


#endif
