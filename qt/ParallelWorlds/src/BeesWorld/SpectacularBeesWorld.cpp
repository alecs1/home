#include "SpectacularBeesWorld.h"
#include "QObject"

// #include <QtDebug>

SpectacularBeesWorld::SpectacularBeesWorld() {
	Bee* aBeePointer;
	for(int i = 0; i < 308; i++) {
		aBeePointer = new Bee("images/BeesWorld/bee.png");
		Speed x(i + 10, i + 10, 0);
		aBeePointer->setSpeed(x);
		aBeePointer->move(Coordinate(0, 300, 0));
		bees << aBeePointer;
		allObjects << aBeePointer;
	}

	for (int i = 0; i < bees.size() - 1; i++) {
		bees[i]->setDestination(bees[i + 1]);
	}

}

//world interface
void SpectacularBeesWorld::step(float seconds) {
	for(int i = 0; i < bees.size(); i++)
		bees[i]->step(seconds);
}

QString SpectacularBeesWorld::getName() {
	return QString(QObject::tr("SpectacularBeesWorld"));
}

QString SpectacularBeesWorld::getDescription() {
	return QString(QObject::tr("A world in which many bees following each other.\nIt starts with a very interesting effect."));
}
