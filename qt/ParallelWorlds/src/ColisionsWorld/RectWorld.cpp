#include "RectWorld.h"

#include <QtDebug>
#include <QColor>

#include "CWRect.h"

#include <cmath>


//good old macros, remove them if when finding anything smarter
#define defaultLifeMacro 3
#define minimumSpawnSizeMacro 5

//magic number, the rect size

RectWorld::RectWorld() {

	CWRect* aRect;

	normalColor = new QColor(0, 255, 0);
	colisionColor = new QColor(255, 0, 0);
	width = 50;
	height = 50;
	defaultLife = defaultLifeMacro;
	minimumSpawnSize = minimumSpawnSizeMacro;

	for(int i = 0; i < 30; i++) {
		Coordinate coord(rand() % 500, rand() % 500);
		Speed speed(rand() % 200, rand() % 200);
		aRect = new CWRect(defaultLife, width, height);
		aRect->move(coord);
		aRect->setSpeed(speed);
		aRect->setColor(*normalColor);
		rects << aRect;
		allObjects << aRect;
		colisionList << 0;
	}

// 	actionAtColision = ETransferSpeed;
// 	actionAtColision = EChangeColor;
	actionAtColision = ETransferSpeedAndConsume;
// 	actionAtColision = ERandom;

	transferType = EToUniqueParent;
}

RectWorld::RectWorld(Coordinate* coord, Speed* speed, float* nWidth, float* nHeight) {
	width = *nWidth;
	height = *nHeight;
	defaultLife = defaultLifeMacro;
	minimumSpawnSize = minimumSpawnSizeMacro;

	actionAtColision = ETransferSpeedAndConsume;

	normalColor = new QColor(0, 255, 0);

	Coordinate newCoord = *coord;
	Speed newSpeed = *speed;
	CWRect* newRect = new CWRect(defaultLife, width / 2, height / 2);

	newCoord.x += width / 4;
	newCoord.y += height / 4;
	newSpeed.x += sqrt(speed->speedModule());
	newSpeed.y += sqrt(speed->speedModule());
	newRect->move(newCoord);
	newRect->setSpeed(newSpeed);
	newRect->setColor(*normalColor);
	rects << newRect;
	allObjects << newRect;
	colisionList << 0;

	newCoord = *coord;
	newSpeed = *speed;
	newRect = new CWRect(defaultLife, width / 2, height / 2);
	newCoord.x -= width / 4;
	newCoord.y += height / 4;
	newSpeed = *speed;
	newSpeed.x -= sqrt(speed->speedModule());
	newSpeed.y += sqrt(speed->speedModule());
	newRect->move(newCoord);
	newRect->setSpeed(newSpeed);
	newRect->setColor(*normalColor);
	rects << newRect;
	allObjects << newRect;
	colisionList << 0;

	newCoord = *coord;
	newSpeed = *speed;
	newRect = new CWRect(defaultLife, width / 2, height / 2);
	newCoord.x -= width / 4;
	newCoord.y -= height / 4;
	newSpeed = *speed;
	newSpeed.x -= sqrt(speed->speedModule());
	newSpeed.y -= sqrt(speed->speedModule());
	newRect->move(newCoord);
	newRect->setSpeed(newSpeed);
	newRect->setColor(*normalColor);
	rects << newRect;
	allObjects << newRect;
	colisionList << 0;

	newCoord = *coord;
	newSpeed = *speed;
	newRect = new CWRect(defaultLife, width / 2, height / 2);
	newCoord.x += width / 4;
	newCoord.y -= height / 4;
	newSpeed = *speed;
	newSpeed.x += sqrt(speed->speedModule());
	newSpeed.y -= sqrt(speed->speedModule());
	newRect->move(newCoord);
	newRect->setSpeed(newSpeed);
	newRect->setColor(*normalColor);
	rects << newRect;
	allObjects << newRect;
	colisionList << 0;

	//qDebug() << "New world from dead object";
}

void RectWorld::step(float seconds) {
	World::step(seconds);


	if (actionAtColision == EChangeColor) {
		//reset the colisionList
		for(int i = 0; i < colisionList.size(); i++)
			colisionList[i] = 0;
	}
	//test for colisions:
	for (int i = 0; i < rects.size(); i++)
		for (int j = i + 1; j < rects.size(); j++) {
			if (GRect::intersect(rects.at(i)->geometricalRect(), rects.at(j)->geometricalRect()) ) {
				if (actionAtColision == ERandom) {
					Speed x(rand() % 200 - 100, rand() % 200 - 100);
					Speed y(rand() % 200 - 100, rand() % 200 - 100);
					allObjects.at(i)->setSpeed(x);
					allObjects.at(j)->setSpeed(y);
				}
				else if (actionAtColision == ETransferSpeed) {
					Speed aux = allObjects.at(i)->getSpeed();
					allObjects.at(i)->setSpeed(allObjects.at(j)->getSpeed());
					allObjects.at(j)->setSpeed(aux);
					//when coliding, some objects get stuck together
					//transfering speeds for an indefinite time
					//to avoid these they will make 2 steps instead of just one:
					allObjects.at(i)->step(seconds);
					allObjects.at(j)->step(seconds);
				}
				else if (actionAtColision == EChangeColor) {
					colisionList[i] = 1;
					colisionList[j] = 1;
				}

				else if (actionAtColision == ETransferSpeedAndConsume) {
					//same actions as for ETransferSpeed
					Speed aux = allObjects.at(i)->getSpeed();
					allObjects.at(i)->setSpeed(allObjects.at(j)->getSpeed());
					allObjects.at(j)->setSpeed(aux);
					allObjects.at(i)->step(seconds);
					allObjects.at(j)->step(seconds);

					rects.at(i)->decreaseLife(1);
					rects.at(j)->decreaseLife(1);
				}
			}


		}

	if (actionAtColision == EChangeColor) {
		for (int i = 0; i < colisionList.size(); i++) {
			if(colisionList.at(i))
				allObjects.at(i)->setColor(*colisionColor);
			else
				allObjects.at(i)->setColor(*normalColor);
			}
	}
	else if(actionAtColision == ETransferSpeedAndConsume) {
		for (int i = 0; i < rects.size(); i++) {
			if (rects.at(i)->isDead()) {
				//create a new world based on the rect that just died
				//save its data first
				Rect* x = rects.at(i);
				Coordinate coord = x->position();
				float width = x->width();
				float height = x->height();
				Speed speed = x->getSpeed();
				//delete it
				delete allObjects.takeAt(i);
				rects.takeAt(i);

				if ( (width + height) / 2 >= minimumSpawnSize) {
					//qDebug() << ((width + height) / 2) << " > " << minimumSpawnSize;
					RectWorld* newWorld = new RectWorld(&coord, &speed, &width, &height);
					newWorld->setUniverseRect(universeRect);

					if (transferType == ENoTransfer) {
						worlds << newWorld;
					}

					else if (transferType == EToUniqueParent) {
						newWorld->giveAwayAll(this);
						delete newWorld;
					}

					else if (transferType == EBySize) {
						qDebug() << "not implemented";
					}

				}

			}
		}

	}
}

void RectWorld::draw(QPainter& painter) {
	World::draw(painter);
}

bool RectWorld::isDead() {
	if ((rects.size() == 0) && (worlds.size() == 0))
		return true;
	else
		return false;
}

void RectWorld::giveAwayAll(RectWorld* receiver) {
// 	qDebug() << "giveAwayAll-start";

	receiver->receiveObjects(allObjects);
	while (not allObjects.empty())
		allObjects.takeLast();

	receiver->receiveRects(rects);
	while (not rects.empty())
		rects.takeLast();

	receiver->receiveWorlds(worlds);
	while (not worlds.empty())
		worlds.takeLast();
}

void RectWorld::receiveObjects(QList<PhysicalObject*> givenObjects) {
	for (int i = 0; i < givenObjects.size(); i++) {
		allObjects << givenObjects[i];
	}
}

void RectWorld::receiveRects(QList<CWRect*> givenRects) {
	for (int i = 0; i < givenRects.size(); i++) {
		rects << givenRects[i];
	}
}

void RectWorld::receiveWorlds(QList<World*> givenWorlds) {
	for (int i = 0; i < givenWorlds.size(); i++) {
		worlds << givenWorlds[i];
	}
}

QString RectWorld::getName() {
	return QString("RectWorld");
}

QString RectWorld::getDescription() {
	return QString("Rectangles that colide (some kind of plastic colision).\nWhen they are too big they explode");
}

//end
