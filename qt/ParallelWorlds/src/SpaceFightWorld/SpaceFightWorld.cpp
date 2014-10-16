#include "SpaceFightWorld.h"

#include <QtDebug>

SpaceFightWorld::SpaceFightWorld() {
	qDebug() << "SpaceFightWorld()";

}

void SpaceFightWorld::step(float seconds) {

}

QString SpaceFightWorld::getName() {
	return QString(QObject::tr("SpaceFightWorld"));
}

QString SpaceFightWorld::getDescription() {
	return QString(QObject::tr("A world where the good ships tryes to fight the bad ships"));
}
