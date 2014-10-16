#include "geometry.h"
#include "Coordinate.h"
#include "Vector.h"

#include "QtDebug"

#include <cmath>

float sqr(float x) {
	return x * x;
}

float distance3D(Coordinate p1, Coordinate p2) {
	return sqrt( sqr(p1.x - p2.x) + sqr(p1.y - p2.y) + sqr(p1.z - p2.z));
}

Vector distanceVector(Coordinate p1, Coordinate p2) {
	Vector result(p2.x - p1.x , p2.y - p1.y, p2.z - p1.z);
	return result;
}

GRect::GRect(Coordinate nCoord1, Coordinate nCoord2) : coord1(nCoord1), coord2(nCoord2) {
// 	//order the points to ease calculations
	if (coord1.x > coord2.x) {
		float aux = coord1.x;
		coord1.x = coord2.x;
		coord2.x = aux;
	}
	if (coord1.y > coord2.y) {
		float aux = coord1.y;
		coord1.y = coord2.y;
		coord2.y = aux;
	}
}

GRect::GRect(Coordinate nCoord1, float width, float height) {
	if ( (width < 0) || (height < 0) ) {
		qDebug() << "GRect::GRect, width and height must be larger than 0";
	}
	coord1 = nCoord1;
	coord2 = Coordinate(coord1.x + width, coord1.y + height);
}

bool GRect::intersects(GRect other){
	return intersect(*this, other);
}

bool GRect::intersect(GRect rect1, GRect rect2) {
	Segment s11(rect1.coord1.x, rect1.coord2.x);
	Segment s12(rect1.coord1.y, rect1.coord2.y);
	Segment s21(rect2.coord1.x, rect2.coord2.x);
	Segment s22(rect2.coord1.y, rect2.coord2.y);
	return  ( (Segment::intersect(s11, s21)) && (Segment::intersect(s12, s22)) );
}

Segment::Segment(float nX1, float nX2) : x1(nX1), x2(nX2) {
	if (x1 > x2) {
		//the points must be in order to ease calculations
		float aux = x1;
		x1 = x2;
		x2 = aux;
	}
}

bool Segment::intersect(Segment s, float p) {
	return ( (p >= s.x1) && (p <= s.x2));
}

bool Segment::intersect(Segment s1, Segment s2) {
	return (intersect(s1, s2.x1) || intersect(s1, s2.x2));
}

//end
