#include "BeesWorld.h"
#include "Bee.h"
#include "QObject" //for QObject::tr()

// #include <QtDebug>

BeesWorld::BeesWorld() {
	Bee* aBeePointer;
	int howMany = 8;
	for(int i = 0; i < howMany; i++) {
		if (i == howMany - 1/*false*/)
            aBeePointer = new Bee("qrc:/images/BeesWorld/bee-nt.png");
		else
            aBeePointer = new Bee("qrc:/images/BeesWorld/bee.png");
		Speed x(i * 17 + 15, i * 19 + 15, 0);
		aBeePointer->setSpeed(x);
		aBeePointer->move(Coordinate(i * 30, i * 40, 0));
		bees << aBeePointer;
		allObjects << aBeePointer;
	}

	for (int i = 0; i < bees.size() - 1; i++) {
		bees[i]->setDestination(bees[i + 1]);
	}

	for(int i = 0; i < bees.size(); i++)
		bees[i]->setRespectRotation(true);

}

//world interface
void BeesWorld::step(float seconds) {
	for(int i = 0; i < bees.size(); i++)
		bees[i]->step(seconds);
}

QString BeesWorld::getName() {
	return QString(QObject::tr("BeesWorld"));
}

QString BeesWorld::getDescription() {
	return QString(QObject::tr("A world in which some bees following each other.\nThe leader has no transparency to show that colisions are respected"));
}
