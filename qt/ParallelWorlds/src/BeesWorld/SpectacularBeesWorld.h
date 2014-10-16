#ifndef SPECTACULARBEESWORLD_H
#define SPECTACULARBEESWORLD_H


#include "Bee.h"
#include "World.h"

#include <QList>

class SpectacularBeesWorld : public World {
public:
	SpectacularBeesWorld();
	void step(float seconds);
	static QString getName();
	static QString getDescription();

private:
	QList<Bee*> bees;

};

#endif
