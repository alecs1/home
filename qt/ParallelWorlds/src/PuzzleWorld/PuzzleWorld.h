#ifndef PUZZLEWORLD_H
#define PUZZLEWORLD_H

#include "World.h"
#include "Coordinate.h"
#include "ui/PuzzleUi.h"

#include <QString>

class QImage;

class PuzzleWorld : public QObject, public World {
	Q_OBJECT
public:
	PuzzleWorld(QString fName = "images/PuzzleWorld/tux-small.png", int nRows = 6, int nCols = 6);

public slots:
	void openImage(QString fName);
	void setRows(int newNr);
	void setColumns(int newNr);
	void makeRandom();
	void solveAll();
	void gather(Coordinate coord = Coordinate(20, 20, 0));
	void gatherCenter();
	void random();
	void randomTogether();

public:
	void reset();
	static QString getDescription();
	static QString getName();

	//not well defined
	void saveState();

private:


protected:
	QString fileName;
	int nrRow, nrCol;
	//I will probably give up some of the pointers
	QImage* image;
	//allObjects and positions should be syncronized (their sizes)
	QList<int> positions;

	PuzzleUi ui;

	int width, height;

};


#endif
