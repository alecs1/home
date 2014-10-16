#ifndef PHYSICALOBJECT_H
#define PHYSICALOBJECT_H


//#include "Alive.h"
#include "Coordinate.h"
#include "Speed.h"
#include <QPainter>

class Universe;

//the object lacks drawing an colision functions and so should not be instantiated;
class PhysicalObject {
	//much functionality in this class, if it will be considered too big split it, if the object size is too big some of the properties could be instantiated only when needed (this means they are pointers to NULL at the beginning)
public:
	PhysicalObject();
	virtual ~PhysicalObject();
	virtual void draw(QPainter& painter) = 0;
	virtual void step(float& seconds);
	virtual void move(Coordinate newCoord);
	virtual void setSpeed(Speed newSpeed);
	virtual void setSpeedModule(float newSpeed); //candidate for renaming
	virtual void setDestination(Coordinate newDest);
	virtual void setDestination(PhysicalObject* object);
	virtual void setColor(int r, int g, int b);
	virtual void setColor(QColor nColor);
	virtual Coordinate destination() const;
		//zero means vX = 1, vY = 0 the objects moves to the right
	float angleXY() const;
	float getSpeedModule() const;
	Speed getSpeed() const;
	virtual void unsetDestination(); //candidate for removal

	void setUniverseRect(QRectF* newUniverseRect);

	virtual bool reachedDestination();
	//probably should make this a reference
	virtual Coordinate position() const;
// 	virtual void setPainter(QPainter* newPainter);

	void setWeigth(float newWeigth);
	float getWeight() const ;

protected:
	virtual void stepNoColideMove(float& seconds); //normal move, with no colision
	virtual void testMarginColide();
	virtual bool testDestination(float seconds);

protected:
	enum RideType {
		EPhysicalRide,
		EToDestination,
		EToObject
	};

// 	Universe* universe;
	QRectF* universeRect;
	Coordinate coord;
	Speed speed;
	QColor color; //maybe this should move to a higher level and not be a property of physical object
	RideType rideType;

	//the destinations should not be created by default, only create them when needed
	Coordinate dest;
	PhysicalObject* destObj;
	bool atDestination;
	float weigth;
};

#endif
