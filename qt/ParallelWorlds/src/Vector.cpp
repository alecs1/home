#include "Vector.h"

#include <cmath>

Vector::Vector(float newX, float newY, float newZ) {
	//all these values must be larger or equal to 0
	x = newX;
	y = newY;
	z = newZ;
}

float Vector::absoluteValue() const {
	return sqrt( x*x + y*y + z*z);
}

bool Vector::isNull() const {
	return ( (x == 0) && (y == 0) && (z == 0) );
}
