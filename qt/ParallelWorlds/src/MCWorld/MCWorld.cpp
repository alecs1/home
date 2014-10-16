#include "MCWorld.h"
#include <QFile>
#include <QTime>
#include <QObject>

// #include <stdlib.h> //se if it is needed on Windows

#include <QtDebug>


TravelingPixmap::TravelingPixmap(QString fName) : Pixmap(fName) {
}

MCWorld::MCWorld() {


	TravelingPixmap* object;

	for(int i = 0; i < 3; i++) {
		object = new TravelingPixmap("images/MCWorld/italian.png");
		italians << object;
		object = new TravelingPixmap("images/MCWorld/maiden.png");
		maidens << object;
	}

	boat = new TravelingPixmap("images/MCWorld/boat.png");

	makeObjectsList();

	shore1 = Coordinate(300, 100);
	shore2 = Coordinate(400, 400);
	boat1 = Coordinate(150, 250);
	boat2 = Coordinate(350, 450);
	setShore1(shore1, boat1);
	setShore2(shore2, boat2);

	preferedSpeed = Speed(60, 60, 0).speedModule();


	reset();
	moving = true;

	//initialise the rides to make the objects move:
	for(int i = 0; i < 3; i++) {
		Ride x;
		x.nrItalians = i + 1;
		x.nrMaidens = i + 1;
		rides << x;
		rides << x;
	}

	//seems like Qt is smart enough to know what to do with the widget:
	ui.show();
	QObject::connect(&ui, SIGNAL(back()), this, SLOT(back()));
	QObject::connect(&ui, SIGNAL(forward()), this, SLOT(forward()));
	QObject::connect(&ui, SIGNAL(fileChosen(QString)), this, SLOT(openFile(QString)));
	QObject::connect(&ui, SIGNAL(speedChanged(double)), this, SLOT(changeSpeed(double)));
	QObject::connect(&ui, SIGNAL(start()), this, SLOT(toggleStart()));
	QObject::connect(&ui, SIGNAL(random()), this, SLOT(random()));
}

MCWorld::~ MCWorld() {
// 	qDebug() << "~MCWorld";
}

//World interface
void MCWorld::step(float seconds) {
	//a state machine, deciding what the objects should do depending on their positions
	if (crtState == EShore1) {
		if(crtRide < rides.size()) {
			//take a new ride
			crtState = EMovingTo2;
// 			qDebug() << "crtRide " << crtRide << rides[crtRide].nrMaidens << rides[crtRide].nrItalians;
			for(int i = 0; i < rides[crtRide].nrMaidens; i++) {
				maidensShore1.last()->setDestination(maidensShore1.last()->shore2);
				travelingMaidens << maidensShore1.last();
				maidensShore1.removeLast();
			}
			for(int i = 0; i < rides[crtRide].nrItalians; i++) {
				italiansShore1.last()->setDestination(italiansShore1.last()->shore2);
				travelingItalians << italiansShore1.last();
				italiansShore1.removeLast();
			}
			boat->setDestination(boat->shore2);
		}
	}

	else if (crtState == EMovingTo2) {
		if (! objectsRiding()) {
			//all objects reached destination
			crtState = EShore2;
			crtRide++;
			for (int i = 0; i < travelingMaidens.size(); i++)
				maidensShore2 << travelingMaidens[i];
			travelingMaidens.clear();
			for (int i = 0; i < travelingItalians.size(); i++)
				italiansShore2 << travelingItalians[i];
			travelingItalians.clear();
		}
	}

	else if (crtState == EShore2) {
		if(crtRide < rides.size() ) {
			crtState = EMovingTo1;
// 			qDebug() << "crtRide " << crtRide << rides[crtRide].nrMaidens << rides[crtRide].nrItalians;
			for(int i = 0; i < rides[crtRide].nrMaidens; i++) {
				maidensShore2.last()->setDestination(maidensShore2.last()->shore1);
				travelingMaidens << maidensShore2.last();
				maidensShore2.removeLast();
			}
			for(int i = 0; i < rides[crtRide].nrItalians; i++) {
				italiansShore2.last()->setDestination(italiansShore2.last()->shore1);
				travelingItalians << italiansShore2.last();
				italiansShore2.removeLast();
			}
			boat->setDestination(boat->shore1);
		}
	}

	else if (crtState == EMovingTo1) {
		if (! objectsRiding()) {
			//all objects reached destination
			crtState = EShore1;
			crtRide++;
			for (int i = 0; i < travelingMaidens.size(); i++)
				maidensShore1 << travelingMaidens[i];
			travelingMaidens.clear();
			for (int i = 0; i < travelingItalians.size(); i++)
				italiansShore1 << travelingItalians[i];
			travelingItalians.clear();
		}
	}

	if (moving) {
		for(int i = 0; i < 3; i++) {
			maidens[i]->step(seconds);
			italians[i]->step(seconds);
		}
		boat->step(seconds);
	}
}

QString MCWorld::getName() {
	return QString(QObject::tr("Missionaries and Cannibals"));
}

QString MCWorld::getDescription() {
	return QString(QObject::tr("A world that simulates the famous problem (with italian men an french maidens this time :)"));
}
//end of world interface

void MCWorld::setShore1(Coordinate peopleShore1, Coordinate boatShore1) {
	for(int i = 0; i < maidens.size(); i++) {
		maidens[i]->shore1 = peopleShore1;
		maidens[i]->setDestination(peopleShore1);
		peopleShore1.x -= 50;
		peopleShore1.y += 20;
		italians[i]->shore1 = peopleShore1;
		italians[i]->setDestination(peopleShore1);
		peopleShore1.x -= 50;
		peopleShore1.y += 20;
	}

	boat->shore1 = boatShore1;
	boat->setDestination(boatShore1);

	boatShore1.y -= 20;
	for(int i = 0; i < maidens.size(); i++) {
		boatShore1.x += 20;
		maidens.at(i)->boat1 = boatShore1;
		boatShore1.x += 20;
		italians.at(i)->boat1 = boatShore1;
	}
}

void MCWorld::setShore2(Coordinate peopleShore2, Coordinate boatShore2) {
	for(int i = 0; i < maidens.size(); i++) {
		maidens.at(i)->shore2 = peopleShore2;
		peopleShore2.x -= 40;
		peopleShore2.y += 20;
		italians.at(i)->shore2 = peopleShore2;
		peopleShore2.x -= 40;
		peopleShore2.y += 20;
	}

	boat->shore2 = boatShore2;

	boatShore2.y -= 20;
	for(int i = 0; i < maidens.size(); i++) {
		boatShore2.x += 20;
		maidens.at(i)->boat2 = boatShore2;
		boatShore2.x += 20;
		italians.at(i)->boat2 = boatShore2;
	}
}

void MCWorld::reset() {
	maidensShore1.clear();
	maidensShore2.clear();
	italiansShore1.clear();
	italiansShore2.clear();
	travelingMaidens.clear();
	travelingItalians.clear();

	for(int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setSpeed(preferedSpeed);
	//a very interesting thing, preferedSpeed is a float, but it transorms in a Speed(preferedSpeed, 0, 0);

	for(int i = 0; i < 3; i++) {
		italians.at(i)->move(italians.at(i)->shore1);
		italians.at(i)->setDestination(italians.at(i)->shore1);
		maidens.at(i)->move(maidens.at(i)->shore1);
		maidens.at(i)->setDestination(maidens.at(i)->shore1);
		maidensShore1 << maidens[i];
		italiansShore1 << italians[i];
	}

	boat->move(boat->shore1);
	crtRide = 0;
	//initialise the shores:
	crtState = EShore1;
}


bool MCWorld::objectsRiding() {
	for(int i = 0; i < travelingMaidens.size(); i++)
		if (! travelingMaidens[i]->reachedDestination())
			return true;

	for (int i = 0; i < travelingItalians.size(); i++)
		if ( ! travelingItalians[i]->reachedDestination())
			return true;

	if (! boat->reachedDestination() )
		return true;

	return false;
}

void MCWorld::makeObjectsList() {
	allObjects << boat;
	for (int i = 0; i < 3; i++)
		allObjects << italians[i];
	for(int i = 0; i < 3; i++)
		allObjects << maidens[i];
}

void MCWorld::openFile(QString fileName) {
// 	qDebug() << "opening file " << fileName;
	QFile file(fileName);
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
         return;
	rides.clear();
	QTextStream inStream(&file);
	while (! inStream.atEnd()) {
		int aux;
		inStream >> aux;
		Ride x;
		x.nrMaidens = aux / 10;
		x.nrItalians = aux % 10;
		rides << x;
		qDebug() << aux << "maidens: " << x.nrMaidens << ",italians: " << x.nrItalians;
	}
	reset();
}

void MCWorld::toggleStart() {
	if (moving)
		moving = false;
	else
		moving = true;
}

void MCWorld::back() {

}

void MCWorld::forward() {

}

void MCWorld::changeSpeed(double newSpeed) {
	for(int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setSpeedModule(newSpeed);
}

void MCWorld::random() {
	rides.clear();
	QTime midnight(0, 0, 0);
	//pd the rand function does not work on Windows, look on this later
	#ifdef Q_OS_WIN32
	//bla something here
	#else
		srand(midnight.secsTo(QTime::currentTime()));
		for(int i = 0; i < allObjects.size(); i++) {
			allObjects[i]->unsetDestination();
			Speed newSpeed(rand() % 600 - 300, rand() % 600 - 300, 0);
			allObjects[i]->setSpeed(newSpeed);
		}
	#endif
}
