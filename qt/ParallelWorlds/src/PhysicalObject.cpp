#include "PhysicalObject.h"
#include "Vector.h"
#include "geometry.h"
#include <math.h>

#include <QtDebug>

PhysicalObject::PhysicalObject() {
// 	qDebug() << "PhysicalObject";
	rideType = EPhysicalRide;
	destObj = NULL;
	universeRect = NULL;
}

PhysicalObject::~PhysicalObject() {
	//keep this as a simple trace of the deletion of the objects
// 	qDebug() << "~PhysicalObject";
}

void PhysicalObject::stepNoColideMove(float& seconds) {
	coord.x += speed.x * seconds;
	coord.y += speed.y * seconds;
}

void PhysicalObject::step(float& seconds) {
	if (rideType == EPhysicalRide) {
		testMarginColide();
		stepNoColideMove(seconds);
	}

	else if (rideType == EToDestination) {
		if (testDestination(seconds)) {
			//reached the destination
			//qDebug() << "have reached destination";
			atDestination = true;
			//Speed zero;
			//setSpeed(zero);
			move(dest);
		}
		else {
			//probably should recalculate apropriate direction each time (this means modyfing the speed values, but leaving the absolute value the same
			Vector newDirection = distanceVector(coord, dest);
			//qDebug() << "new Direction: " << newDirection.x << newDirection.y << newDirection.z;
			speed.setVectorDirection(newDirection);
			stepNoColideMove(seconds);
		}
	}

	else if (rideType == EToObject) {
		Vector newDirection = distanceVector(coord, destObj->position());
// 		qDebug() << "new Direction: " << newDirection.x << newDirection.y << newDirection.z;
		speed.setVectorDirection(newDirection);
		stepNoColideMove(seconds);
	}
}


// void PhysicalObject::setPainter(QPainter* newPainter) {
// 	qDebug() << "PhysicalObject::setPainter()";
// 	if(painter == NULL) {
// 		qDebug() << "Error: PhysicalObject::setPainter(): newPainter == NULL";
// 		return;
// 	}
// 	painter = newPainter;
// 	qDebug() << painter;
// }

void PhysicalObject::move(Coordinate newCoord) {
	//qDebug() << "PhysicalObject::move()";
	coord = newCoord;
}

void PhysicalObject::setSpeed(Speed newSpeed) {
	rideType = EPhysicalRide;
	speed = newSpeed;
}

void PhysicalObject::setSpeedModule(float newSpeed) {
	speed.setModule(newSpeed);
}

void PhysicalObject::setDestination(Coordinate newDest) {
	rideType = EToDestination;
	dest = newDest;
	atDestination = false;
}

void PhysicalObject::setDestination(PhysicalObject* object) {
	rideType = EToObject;
	destObj = object;
}

Coordinate PhysicalObject::destination() const {
	if (rideType == EPhysicalRide)
		return coord;
	else if (rideType == EToDestination)
		return dest;
	else if (rideType == EToObject)
		return destObj->position();
	else {
		qDebug() << "no such rideType" << rideType;
		return Coordinate(0, 0, 0);
	}
}

float PhysicalObject::angleXY() const {
	float radian = 3.14159265 / 180.0; //use it as it is now, it is not correct
	float x = speed.x;
	float y = speed.y;
	if (y == 0)
		y = 0.00001;
// 	qDebug() << atan(x/y) / radian;
// 	for(float i = -20; i <  20; i += 0.1)
// 		qDebug() << "atan(" << i << ") = " << atan(i) /radian;
	//for a period of 2 pi tangent has two places for each of it values, so different angles must be returned from the knowledge we have from x and y; do observe that we input 2 values and only get one, information is lost; also remember the graph of the arc tangent function

	if ( y > 0)
		return atan( - x/y) / radian + 180;
	else
		return atan(- x/y) / radian;
}

float PhysicalObject::getSpeedModule() const {
	return speed.speedModule();
}

Speed PhysicalObject::getSpeed() const {
	return speed;
}

void PhysicalObject::setUniverseRect(QRectF* newUniverseRect) {
	if (newUniverseRect == NULL)
		qDebug() << "PhysicalObject::setUniverseRect, newUniverseRect is NULL";
	universeRect = newUniverseRect;
// 	qDebug() << "setted universeRect: " << *universeRect;
}

//if the distance to the target is smaller or equal than one step then the destination has been reached because we need at most one step until there
//to make this more spectacular smaller distances could be considered, but this is the mathematicaly correct idea
bool PhysicalObject::testDestination(float seconds) {
	//qDebug() << distance3D(coord, dest);
	if ( distance3D(coord, dest) <= (speed.speedModule() * seconds) / 1 )
		return true;
	return false;
}

void PhysicalObject::testMarginColide() {
	//to be implemented in specifical ways
	//in the current implementation the objects ask their parent universe for its size, this should be changed, and the objects and their world should be notified about the change

	//anyway, in order to test the colision, you must know the limits of the surrounding world/universe:
	if (universeRect == NULL) {
		qDebug() << "testMarginColide with universeRect = NULL";
	}
}

bool PhysicalObject::reachedDestination() {
	return atDestination;
}

//change this
void PhysicalObject::unsetDestination() {
	rideType = EPhysicalRide;
	destObj = NULL;
}

Coordinate PhysicalObject::position() const {
	return coord;
}

void PhysicalObject::setWeigth(float newWeigth) {
	weigth = newWeigth;
}

float PhysicalObject::getWeight() const {
	return weigth;
}

void PhysicalObject::setColor(int r, int g, int b) {
	color.setRgb(r, g, b);
}

void PhysicalObject::setColor(QColor nColor) {
	color = nColor;
}
