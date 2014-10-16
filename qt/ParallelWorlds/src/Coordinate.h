#ifndef COORDINATE_H
#define COORDINATE_H

class Coordinate {
public:
	Coordinate(float newX = 0, float newY = 0, float newZ = 0) {
		x = newX;
		y = newY;
		z = newZ;
	}
public:
	float x, y, z;
};

#endif
