#ifndef BEESWORLD_H
#define BEESWORLD_H

#include "World.h"

#include <QList>

class Bee;

class BeesWorld : public World {
public:
	BeesWorld();
	void step(float seconds);
	static QString getName();
	static QString getDescription();

private:
	QList<Bee*> bees;

};

#endif
