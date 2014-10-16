#include "PuzzleUi.h"

#include <QFileDialog>

PuzzleUi::PuzzleUi(QWidget* parent) : QWidget(parent) {
	setupUi(this);

	connect(openButton, SIGNAL(clicked()), this, SLOT(openImage()));
	connect(selectPuzzleButton, SIGNAL(clicked()), this, SLOT(openPuzzle()));
	connect(randPozButton, SIGNAL(clicked()), this, SIGNAL(makePuzzle()));
	connect(randMoveButton, SIGNAL(clicked()), this, SIGNAL(moveRandomly()));
	connect(solveButton, SIGNAL(clicked()), this, SIGNAL(solve()));
	connect(parallelSolveButton, SIGNAL(clicked()), this, SIGNAL(solveParallel()));
	connect(solutionSolveButton, SIGNAL(clicked()), this, SLOT(openSolution()));
	connect(rowsSpinBox, SIGNAL(valueChanged(int)), this, SIGNAL(changeNrRow(int)));
	connect(colsSpinBox, SIGNAL(valueChanged(int)), this, SIGNAL(changeNrCol(int)));
	connect(gatherButton, SIGNAL(clicked()), this, SIGNAL(gather()));
	connect(randGatherButton, SIGNAL(clicked()), this, SIGNAL(moveRandomlyTogether()));

	openedSolution = false;
}

void PuzzleUi::openImage() {
	QString fileName = QFileDialog::getOpenFileName(this, "Select an image", imageFolder);
	if (fileName != "") {
		imageFolder = fileName;
		emit fileChosen(fileName);
	}
}

void PuzzleUi::openSolution() {
	QString fileName = QFileDialog::getOpenFileName(this, "Select a solution", solutionFolder);
	if (fileName != "") {
		solutionFolder = fileName;
		emit solveFromSolution(fileName);
	}
}

void PuzzleUi::openPuzzle() {
	QString fileName = QFileDialog::getOpenFileName(this, "Select a puzzle", puzzleFolder);
	if (fileName != "") {
		puzzleFolder = fileName;
		emit puzzleChosen(fileName);
	}
}

//end
