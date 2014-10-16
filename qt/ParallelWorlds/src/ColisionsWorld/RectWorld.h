#ifndef RECTWORLD_H
#define RECTWORLD_H

#include "World.h"

class CWRect;
class QColor;
class Coordinate;
class Speed;

class RectWorld : public World {
public:
	RectWorld();
	RectWorld(Coordinate* coord, Speed* speed, float* nWidth, float* nHeight);
	static QString getName();
	static QString getDescription();
	void step(float seconds);
	void draw(QPainter& painter);
	bool isDead();

	//receives a command to give all the objects and the worlds to the sender
	void giveAwayAll(RectWorld* receiver);
	//could be made more efficient
	void receiveObjects(QList<PhysicalObject*> givenObjects);
	void receiveRects(QList<CWRect*> givenRects);
	void receiveWorlds(QList<World*> givenWorlds);

private:
	QList<CWRect*> rects;
	QList<int> colisionList;
	float width, height;
	int defaultLife;
	float minimumSpawnSize;

	enum EAction {
		ERandom,
		ETransferSpeed,
		EChangeColor,
		ETransferSpeedAndConsume,
		EPlasticColision,

	};
	EAction actionAtColision;

	enum EObjectTransferType {
		ENoTransfer,
		EToUniqueParent,
		EBySize,
	};
	EObjectTransferType transferType;

	QColor* normalColor;
	QColor* colisionColor;


};

#endif
