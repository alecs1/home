#include "PuzzleWorld.h"
#include "Pixmap.h"

#include <QTime> //for some randomness

#include <QImage>
#include <QDebug>

PuzzleWorld::PuzzleWorld(QString fName, int nRows, int nCols):
fileName(fName), nrRow(nRows), nrCol(nCols) {
	image = new QImage;

	//at this time there is no universeRect, nobody setted it yet, solve this somehow
	openImage(fileName);

	ui.show();

	//connections:
	connect(&ui, SIGNAL(fileChosen(QString)), this, SLOT(openImage(QString)));
	connect(&ui, SIGNAL(changeNrRow(int)), this, SLOT(setRows(int)));
	connect(&ui, SIGNAL(changeNrCol(int)), this, SLOT(setColumns(int)));
	connect(&ui, SIGNAL(makePuzzle()), this, SLOT(makeRandom()));
	connect(&ui, SIGNAL(solveParallel()), this, SLOT(solveAll()));
	connect(&ui, SIGNAL(gather()), this, SLOT(gatherCenter()));
	connect(&ui, SIGNAL(moveRandomly()), this, SLOT(random()));
	connect(&ui, SIGNAL(moveRandomlyTogether()), this, SLOT(randomTogether()));

	QTime midnight(0, 0, 0);
	srand(midnight.secsTo(QTime::currentTime()));
}

void PuzzleWorld::openImage(QString fName) {
// 	qDebug() << "openImage:" << fName;
	fileName = fName;

	image->load(fName);

	//for the beginning clean everything
	while(! allObjects.empty() )
		delete(allObjects.takeLast());

	//compute the sizes, suppose the sizes divide perfectly
	width = image->width() / nrCol;
	height = image->height() / nrRow;

	QImage auxImage;
	for (int i = 0; i < nrRow; i++)
		for(int j = 0; j < nrCol; j++) {
			auxImage = image->copy(j * width, i * height, width, height);
			Pixmap* pixmap = new Pixmap(auxImage);
			allObjects << pixmap;
		}

	//also reset every speed and tell the objects which is the universeRect
// 	qDebug() << "PuzzleWorld::PuzzleWorld, setting universeRect: " << universeRect;
	if (universeRect != NULL)
		World::setUniverseRect(universeRect);

	for (int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setSpeed(Speed(65, 65, 0));
};

void PuzzleWorld::setRows(int newNr) {
	nrRow = newNr;
	openImage(fileName);
}

void PuzzleWorld::setColumns(int newNr) {
	nrCol = newNr;
	openImage(fileName);
}

QString PuzzleWorld::getName() {
	return QString("PuzzleWorld");
}

QString PuzzleWorld::getDescription() {
	return QString("The objects are formed from a main image.\nLoad a puzzle and solution and see how it works");
}

void PuzzleWorld::makeRandom() {
	positions.clear();
	for(int i = 0; i < allObjects.size(); i++)
		positions << i;

	//now some randomizing code


	for(int i = 0; i < 3 * allObjects.size(); i++) {
		//swap two of the values from the vector at a time
		int x = rand() % positions.size();
		int y = rand() % positions.size();
		int aux = positions[x];
		positions[x] = positions[y];
		positions[y] = aux;
	}

	//now place the pictures in their positions
	int xmargin = width/2 + 0;
	int ymargin = height/2 + 0;
	for(int i = 0; i < positions.size(); i++) {
		int y = positions[i] / nrCol; //find out the row
		int x = positions[i] % nrCol; //find out the column
		allObjects[i]->setDestination(Coordinate(x * width + xmargin, y * height + ymargin, 0));
	}
};

void PuzzleWorld::solveAll() {
	int xmargin = width/2 + 0;
	int ymargin = height/2 + 0;
	for(int i = 0; i < allObjects.size(); i++) {
		int y = i / nrCol; //find out the row
		int x = i % nrCol; //find out the column
		allObjects[i]->setDestination(Coordinate(x * width + xmargin, y * height + ymargin, 0));
	}
}

void PuzzleWorld::gather(Coordinate coord) {
	//move all the objects in the center of the image:

	for(int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setDestination(coord);
}

void PuzzleWorld::gatherCenter() {
	int x = nrCol * width / 2;
	int y = nrRow * height / 2;
	gather(Coordinate(x, y));
}

void PuzzleWorld::random() {
	for (int i = 0; i < allObjects.size(); i++) {
		allObjects[i]->unsetDestination();
		allObjects[i]->setSpeed(Speed(rand() % 400 - 200, rand() % 400 - 200, 0));
	}
}

void PuzzleWorld::randomTogether() {
	Speed x(rand() % 400 - 200, rand() % 400 - 200, 0);
	for(int i = 0; i < allObjects.size(); i++) {
		allObjects[i]->unsetDestination();
		allObjects[i]->setSpeed(x);
	}
}
//end
