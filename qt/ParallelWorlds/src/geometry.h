#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "Coordinate.h"

class Vector;

//move this in another place:
float sqr(float x);
float distance3D (Coordinate p1, Coordinate p2);
Vector distanceVector(Coordinate p1, Coordinate p2);

//geometry classes:
//optimisation candidates,many function calls

//a Rectagle in the oXY plane, not a 3d one. Qt-ish coordinate system, parallel with the axes,
class GRect {
public:
	GRect(Coordinate nCoord1 = Coordinate(0, 0), Coordinate nCoord2 = Coordinate(0, 0));
	GRect(Coordinate nCoord1, float width, float height);
	bool intersects(GRect other);
	static bool intersect(GRect rect1, GRect rect2);

public:
	Coordinate coord1, coord2; //the upper left point
};

class Segment {
public:
	Segment(float nX1, float nX2);
	bool intersects(Segment other);
	static bool intersect(Segment s1, Segment s2);
	static bool intersect(Segment s, float p);

public:
	float x1, x2;
};

#endif
