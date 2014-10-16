#include "SimpleBeesWorld.h"

#include <QtDebug>

SimpleBeesWorld::SimpleBeesWorld() {
// 	qDebug() << "SimpleBeesWorld()";

	Bee* aBeePointer;
	for(int i = 0; i < 1; i++) {
		aBeePointer = new Bee("images/BeesWorld/simple-bee.png");
		Speed x(i*15 + 59, 2*i + 47, 0);
		aBeePointer->setSpeed(x);
		bees << aBeePointer;
		allObjects << aBeePointer;
	}

}

//world interface
QString SimpleBeesWorld::getName() {
	return QString(QObject::tr("SimpleBeesWorld"));
}

QString SimpleBeesWorld::getDescription() {
	return QString(QObject::tr("Some Bees bumping in the walls"));
}
