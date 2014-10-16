#ifndef SPEED_H
#define SPEED_H

class Vector;

//speed resembles a vector very much, try to make speed a vector or related to a vector
class Speed {
	//holds the speed of an object in m/s (pixels per second for the moment :P )
public:
	Speed(float newX = 0, float newY = 0, float newZ = 0);
	float speedModule() const;

	//this one alters the real module of the speed. The real speed should keep a a value of the original speed that is set at construction or with setModule and consult it at each computation
	void setVectorDirection(Vector newVector);

	void setModule(float newModule); //candidate for renaming
public:
	float x, y, z;
};

#endif
