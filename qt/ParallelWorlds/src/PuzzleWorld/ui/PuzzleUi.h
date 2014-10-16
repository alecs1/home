#ifndef PUZZLEUI_H
#define PUZZLEUI_H

#include "ui_puzzle.h"

#include <QWidget>

class PuzzleUi : public QWidget, public Ui::PuzzleForm {
	Q_OBJECT

public:
	PuzzleUi(QWidget* parent = 0);

private slots:
	void openImage();
	void openSolution();
	void openPuzzle();
// 	void solveFromSolution();

signals:
	void fileChosen(QString);
	void puzzleChosen(QString);
	void newRow(int);
	void newCol(int);
	void makePuzzle(); //to mix (a amesteca) the pieces
	void moveRandomly();
	void solve();
	void solveParallel();
	void solveFromSolution(QString);
	void changeNrRow(int);
	void changeNrCol(int);
	void gather();
	void moveRandomlyTogether();


private:
	QString imageFolder;
	QString puzzleFolder;
	QString solutionFolder;
	bool openedSolution;

};

#endif
