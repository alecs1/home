#ifndef SIMPLEBEESWORLD_H
#define SIMPLEBEESWORLD_H


#include "Bee.h"
#include "World.h"

#include <QList>

class SimpleBeesWorld : public World {
public:
	SimpleBeesWorld();
	static QString getName();
	static QString getDescription();

private:
	QList<Bee*> bees;

};

#endif
