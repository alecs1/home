#ifndef UNIVERSE_H
#define UNIVERSE_H

#include <QPainter>

#include "WorldControlInterface.h"

class World;
class WorldEmisar;
class ListerInterface;
class WorldBuddy;

//this Universe is just a more sophisticated World
class Universe : public WorldControlInterface {
// 	Q_OBJECT
public:
	Universe();
	~Universe();
	void draw(QPainter& painter);
	void setWidget(QWidget* nWidget);
	void setListerInterface(ListerInterface* newLister);
	void setRect(QRectF* newUniverseRect);
	int getNumberOfObjects() const;
// 	void testLeaks();

	//interface for the inside objects:
// 	QRectF getContentsRect();

private:
	QWidget* widget; //the widget that will be painting what the universe wants, must be received from outside, if not received drawing will not be called
	ListerInterface* listerInterface;

	QTime* time;
	float elapsedSinceLastUpdate;

	QList<float> fpsList;
	float medFps,
		crtFps;
	QString lastFpsGot,
		last50Fps;
	QString auxString;
	int fpsListSize;

public:
	void step();
	//WorldControlInterface
	bool addWorld(QString worldName);
	bool removeWorld(QString worldName);

private:
	//this function has to be updated manually, I will try to change this
	World* makeWorldInstance(QString worldName);
	void addEmisar(QString name, QString description);

//lists
private:
	int count; //this will personalize the new worlds created, it is limited by the dimension of an int :)
	QRectF* universeRect;
	QList <World*> worlds;
	QList <WorldEmisar*> emisars;
	QList <WorldBuddy*> buddies;

//things to come:
//private:
	//void drawRegion();
	//void drawObject(int i);
	//void setSteppingTime(float seconds);
	//bool forceStep();
};

#endif
