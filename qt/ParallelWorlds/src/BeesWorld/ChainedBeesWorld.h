#ifndef CHAINEDBEESWORLD_H
#define CHAINEDBEESWORLD_H

#include "BeesWorld.h"

class ChainedBeesWorld : public BeesWorld {
public:
	static QString getName();
	static QString getDescription();
	void draw(QPainter& painter);
};

#endif
