#include "ChainedBeesWorld.h"
#include "PhysicalObject.h"
#include <QtDebug>

void ChainedBeesWorld::draw(QPainter& painter) {
	//have a list of objects, all of them should receive the draw command
	Coordinate c1, c2;
	for (int i = 0; i < allObjects.size() - 1; i++) {
		allObjects.at(i)->draw(painter);
		c1 = allObjects.at(i)->position();
		c2 = allObjects.at(i)->destination();
		//candidate for optimisation (not constructing the points inside the loop)
		painter.drawLine(QPointF(c1.x, c1.y), QPointF(c2.x, c2.y));
	}
	allObjects.last()->draw(painter);
}

QString ChainedBeesWorld::getName() {
	return QString("Chained Bees");
}

QString ChainedBeesWorld::getDescription() {
	return QString("The same bees that follow each other, this time chained");
}
