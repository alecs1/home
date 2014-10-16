#ifndef SPACEFIGHTWORLD
#define SPACEFIGHTWORLD

#include "World.h"

class SpaceShip;

class SpaceFightWorld : public World {

public:
	SpaceFightWorld();
	void step(float seconds);
	static QString getName();
	static QString getDescription();

};



#endif
