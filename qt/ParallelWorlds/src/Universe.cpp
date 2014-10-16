#include "Universe.h"
#include "ListerInterface.h"

#include "WorldEmisar.h"
#include "WorldBuddy.h"

//worlds
#include "MCWorld/MCWorld.h"
#include "BeesWorld/BeesWorld.h"
#include "BeesWorld/ChainedBeesWorld.h"
#include "BeesWorld/SpectacularBeesWorld.h"
#include "BeesWorld/SimpleBeesWorld.h"
#include "PuzzleWorld/PuzzleWorld.h"
#include "PuzzleWorld/PuzzleWorldShow.h"
#include "ColisionsWorld/RectWorld.h"
#include "SpaceFightWorld/SpaceFightWorld.h"

#include <QDebug>
#include <QTime>

Universe::Universe() {

	//a new problem has arised, I don't know how to create a class without writing it's name in the source code
	//1.I would use pointers to those classes constructors
	//2.another solution would be the use of templates
	//3.the solution I will use this time is to make create a new method which has to be updated manually
	count = 0;

	addEmisar(MCWorld::getName(), MCWorld::getDescription());
	addEmisar(BeesWorld::getName(), BeesWorld::getDescription());
	addEmisar(ChainedBeesWorld::getName(), ChainedBeesWorld::getDescription());
	addEmisar(SimpleBeesWorld::getName(), SimpleBeesWorld::getDescription());
	addEmisar(SpectacularBeesWorld::getName(), SpectacularBeesWorld::getDescription());
	addEmisar(PuzzleWorld::getName(), PuzzleWorld::getDescription());
	addEmisar(PuzzleWorldShow::getName(), PuzzleWorldShow::getDescription());
	addEmisar(RectWorld::getName(), RectWorld::getDescription());
	addEmisar(SpaceFightWorld::getName(), SpaceFightWorld::getDescription());

	fpsListSize = 200;
	for(int i = 0; i < fpsListSize; i++)
		fpsList.append(0);
	medFps = 0;
	time = new QTime;
	time->start();

	universeRect = NULL;

// 	qDebug() << "universe created";
}

Universe::~Universe() {
	for(int i = 0; i < worlds.size(); i++)
		delete worlds.at(i);

	for(int i = 0; i < emisars.size(); i++)
 		delete emisars.at(i);

	for(int i = 0; i < buddies.size(); i++)
		delete buddies.at(i);

	qDebug() << "~Universe";
}

void Universe::draw(QPainter& painter) {
	painter.setPen(Qt::NoPen);
	for (int i = 0; i < worlds.size(); i++)
		worlds.at(i)->draw(painter);

	painter.setPen(Qt::SolidLine);

	crtFps = 1.0 / elapsedSinceLastUpdate;
	lastFpsGot = "Crt fps: " + auxString.setNum(crtFps);


	painter.drawText(QPointF(20, 40), lastFpsGot);

	//if crtFps is too big compared to the ones before we skip it
	if(crtFps < medFps  + 5000 ) {
		medFps -= fpsList.takeFirst();
		fpsList.append(crtFps);
		medFps += crtFps;
	}

	last50Fps = "Last 200 fps average: " + auxString.setNum(medFps / fpsListSize);;
	painter.drawText(QPointF(20, 60), last50Fps);
}

void Universe::setWidget(QWidget* nWidget) {
	widget = nWidget;
}

void Universe::setListerInterface(ListerInterface* newLister) {
	listerInterface = newLister;
	for (int i = 0; i < emisars.size(); i++)
		listerInterface->addEmisar(emisars[i]);

// 	addWorld(BeesWorld::getName());
}

void Universe::step() {
// 	qDebug() << "Universe::step";
	//qDebug() << getNumberOfObjects();
	elapsedSinceLastUpdate = float(time->elapsed()) / 1000.0;
	time->restart();
	for (int i = 0; i < worlds.size(); i++)
		worlds.at(i)->step(elapsedSinceLastUpdate);

	widget->update();
}

bool Universe::addWorld(QString worldName) {
	World* newWorld = makeWorldInstance(worldName);
	if (newWorld != NULL) {
		//move the setUniverse into the world's constructor
		count++;
		QString identifier; identifier.setNum(count);

		newWorld->setUniverseRect(universeRect);
		worlds << newWorld;
		WorldBuddy* newBuddy = new WorldBuddy(this, newWorld, worldName + identifier);
		listerInterface->addBuddy(newBuddy);
		buddies << newBuddy;
		return true;
	}

	return false;
}

bool Universe::removeWorld(QString worldName) {
	for(int i = 0; i < buddies.size(); i++)
		if(buddies[i]->getName() == worldName) {

			for(int j = 0; j < worlds.size(); j++)
				if(worlds[j] == buddies[i]->getWorldPointer() )
					delete worlds.takeAt(j);

			listerInterface->removeBuddy(worldName);
			delete buddies.takeAt(i);
		}
	return false;
}

//this could probably be replaced also with templates if no other smarter methods are found
World* Universe::makeWorldInstance(QString worldName) {
	if(worldName == MCWorld::getName() )
		return new MCWorld;

	else if(worldName == BeesWorld::getName() )
		return new BeesWorld;

	else if(worldName == ChainedBeesWorld::getName())
		return new ChainedBeesWorld;

	else if(worldName == SimpleBeesWorld::getName() )
		return new SimpleBeesWorld;

	else if(worldName == SpectacularBeesWorld::getName())
		return new SpectacularBeesWorld;

	else if (worldName == PuzzleWorld::getName())
		return new PuzzleWorld;

	else if (worldName == PuzzleWorldShow::getName())
		return new PuzzleWorldShow;

	else if (worldName == RectWorld::getName())
		return new RectWorld;

	else if (worldName == SpaceFightWorld::getName())
		return new SpaceFightWorld;

	qDebug() << "Error, no such world type: " << worldName << ". Nothing was created";
	return NULL;
}

void Universe::addEmisar(QString name, QString description) {
	//should test that something is not added twice (most probably because some world was not renamed
	WorldEmisar* newEmisar = new WorldEmisar(this, name, description);
	emisars << newEmisar;
}

void Universe::setRect(QRectF* newUniverseRect) {
	universeRect = newUniverseRect;
	for(int i = 0; i < worlds.size(); i++)
		worlds[i]->setUniverseRect(universeRect);
// 	qDebug() << "Universe()::setRect() - end " << *newUniverseRect;
}

int Universe::getNumberOfObjects() const {
	int nr = 0;
	for (int i = 0; i < worlds.size(); i++)
		nr += worlds[i]->getNumberOfObjects();
	return nr;
}

// void Universe::testLeaks() {
// 	for (int i = 0; i
//
// }

// void Universe::setPainter(QPainter* newPainter) {
// 	qDebug() << "Universe::setPainter()";
// 	if(newPainter == NULL) {
// 		qDebug() << "Error: Universe::setPainter(): newPainter == NULL";
// 		return;
// 	}
// 	for(unsigned int i = 0; i < objects.size(); i++)
// 		objects.at(i)->setPainter(newPainter);
// }
