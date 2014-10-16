#ifndef PUZZLEWORLDSHOW_H
#define PUZZLEWORLDSHOW_H

#include "PuzzleWorld.h"

#include <QTime>

class PuzzleWorldShow : public PuzzleWorld {
public:
	PuzzleWorldShow();
	void step(float seconds);
	static QString getName();
	static QString getDescription();

	enum States {
		EGoingCenter1,
		ESolve1,
		EImplode1,
		ESolve2,
		EHalfImplode1,
		EMoveTogether1, //move together for 20 seconds
		EShakeTogether1, //the group will change its direction at every step
		EMix1,
		ESolve3,
		EExplode1,
		EMix2,
		ESolve4,
		EExplode2,
		ESolve5,
		ESlowlyDecompose1,
		EImplode2,
		EExplode3,
		EFollowing1,

	};

	States state;

private:
	QTime timer;
	int elapsedTime;
	static const int secondDivisions = 1000;
	int slowlyDecomposeCounter;
	int episode;


};

#endif
