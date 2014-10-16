#ifndef MCWORLD_H
#define MCWORLD_H

#include "Pixmap.h"
#include "World.h"

#include "ui/MaidensUi.h"

class TravelingPixmap : public Pixmap {
public:
	TravelingPixmap(QString fName = "");
	//Coordinate origin, destination;
	Coordinate shore1, shore2, boat1, boat2;
};

struct Ride {
	int nrItalians, nrMaidens;
};

//take care. Multiple inheritance, more than that, QObject must be the first in the inheritance list
class MCWorld : public QObject, public World{
	Q_OBJECT

public:
	MCWorld();
	~MCWorld();

	void step(float seconds);

	static QString getName();
	static QString getDescription();


private:
	void setShore1(Coordinate peopleShore1, Coordinate boatShore1);
	void setShore2(Coordinate peopleShore2, Coordinate boatShore2);
	void reset();
	bool objectsRiding();
	void makeObjectsList();
	MaidensUi ui;

private:
	//try to put these objects in a single vector (serialize acces to them somehow)
	QList<TravelingPixmap*> maidens;
	QList<TravelingPixmap*> italians;
	TravelingPixmap* boat;

	QList<Ride> rides;
	int crtRide;

	//and now a beautiful state machine
	enum States {
		EMovingTo1,
		EMovingTo2,
		EShore1,
		EShore2
	};

	States crtState;
	bool moving;
	float preferedSpeed;

	QList<TravelingPixmap*> maidensShore1;
	QList<TravelingPixmap*> maidensShore2;
	QList<TravelingPixmap*> italiansShore1;
	QList<TravelingPixmap*> italiansShore2;
	QList<TravelingPixmap*> travelingMaidens;
	QList<TravelingPixmap*> travelingItalians;

	Coordinate shore1, shore2, boat1, boat2;

//for the user interface
private slots:
	void openFile(QString file);
	void toggleStart();
	void back();
	void forward();
	void changeSpeed(double newSpeed);
	void random();
};

// class MCWorldEmisar : public WorldEmisar {
// public:
// 	MCWorldEmisar(QString nName = QObject::tr("Missionaries and Cannibals"),
// 	              QString nDescription = QObject::tr("A world that simulates the famous problem (with italian men an french maidens this time :) )"));
// 	/*MCWorld**/ void makeInstance();
// };

#endif
