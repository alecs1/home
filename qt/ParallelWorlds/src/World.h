#ifndef WORLD_H
#define WORLD_H

#include <QList>
#include <QString>

//#include "Alive.h"
//#include "PhysicalObject.h"

class QSize;
class QPainter;
class QRectF;
class PhysicalObject;

class World/* : public Alive*/{
public:
	World();
	World(World* parent, int indexHint = 0);
	virtual ~World();
	//void childDied(Alive* child, int indexHint = 0);
	virtual void setSize(QSize newSize);
	virtual void scale(QSize newSize);
	virtual void setUniverseRect(QRectF* newUniverseRect);
// 	virtual void setUniverse(Universe* newUniverse); //no more needing to pass a parameter to the function call
	virtual void draw(QPainter& painter);
	virtual void drawGL(); //to be implemented
	virtual void step(float seconds); //candidate to be pure virtual
	int getNumberOfObjects() const;
	static QString getName();
	static QString getDescription();

protected:
	QRectF* universeRect;
	QList<PhysicalObject*> allObjects;
	QList<World*> worlds;
};

#endif
