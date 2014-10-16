#include "World.h"

#include "PhysicalObject.h"

#include <QtDebug>

World::World() {
	universeRect = NULL;
}

World::World(World* parent, int indexHint)/* : Alive(parent, indexHint) */{

}

World::~World() {
	while(! allObjects.empty())
		delete allObjects.takeLast();

	while(! worlds.empty())
		delete worlds.takeLast();
// 	qDebug() << "~World";
}

// void World::childDied(Alive* child, int indexHint) {
// 	//trace the
//
// }

void World::setSize(QSize newSize) {
	//not important, the objects should find their size
}

void World::scale(QSize newSize) {
	//if the size of the universe changes and everything should scale (objects sizes, their speed and destinations. Kind of useless, don't know if it makes sense and hard to implement
}

void World::setUniverseRect(QRectF* newUniverseRect) {
	if (newUniverseRect == NULL) {
		qDebug() << "World::setUniverseRect, newUniverseRect is NULL, did nothing";
		return;
	}
	universeRect = newUniverseRect;

	for(int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setUniverseRect(universeRect);

	for(int i = 0; i < worlds.size(); i++)
		worlds[i]->setUniverseRect(newUniverseRect);
	//	qDebug() << "setted universeRect: " << *newUniverseRect;
}

void World::draw(QPainter& painter) {
	//have a list of objects, all of them should receive the draw command
	for (int i = 0; i < allObjects.size(); i++)
		allObjects.at(i)->draw(painter);

	for (int i = 0; i < worlds.size(); i++)
		worlds.at(i)->draw(painter);
}

void World::drawGL() {
	qDebug() << "Word::drawGL - not implemented";
}

void World::step(float seconds) {
	//implement mode to control the movement of the controled objects, then tell the objects in the list to move
	//it is OK not to have it pure virtual

	for(int i = 0; i < allObjects.size(); i++)
		allObjects.at(i)->step(seconds);
// 	qDebug() << "objects stepped";

	for(int i = 0; i < worlds.size(); i++)
		worlds.at(i)->step(seconds);
// 	qDebug() << "worlds stepped";
}

int World::getNumberOfObjects() const {
	int nr = allObjects.size();
	for (int i = 0; i < worlds.size(); i++)
		nr += worlds[i]->getNumberOfObjects();
	return nr;
}

QString World::getName() {
	return QString("Base Class World");
}

QString World::getDescription() {
	return QString("the static World::getDescription function should have been reimplemented");
}
