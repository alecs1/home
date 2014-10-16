#ifndef VECTOR_H
#define VECTOR_H

//for the moment don''t inherit BaseClass anymore, since no advantages have been found
class Vector {
public:
	Vector(float newX, float newY, float newZ);
	float absoluteValue() const;
	bool isNull() const;

public:
	float x, y, z;
};
#endif
