#include "PuzzleWorldShow.h"
#include "PhysicalObject.h"

#include <QtDebug>

//the default image is a bit offending to show it to the teachers
// PuzzleWorldShow::PuzzleWorldShow() : PuzzleWorld("images/PuzzleWorld/yousmile.png", 60, 60) {
PuzzleWorldShow::PuzzleWorldShow() : PuzzleWorld("images/PuzzleWorld/tux-medium.png", 60, 60) {

// 	ui.hide();

	for(int i = 0; i < allObjects.size(); i++)
		allObjects[i]->setDestination(Coordinate(700, 500));

	state = EGoingCenter1;
// 	state = EExplode3;
	episode = 0;
	timer.start();

}

void PuzzleWorldShow::step(float seconds) {
	//a state machine must be implemented

	if (state == EGoingCenter1) {
		//find out if the first object has reached some point, say 400/400 and make set the next state
		if (allObjects.at(allObjects.size() * 95 / 100)->reachedDestination()) {
			state = ESolve1;
			solveAll();
		}
	}
	else if (state == ESolve1) {
		if(allObjects.at(allObjects.size() * 95 / 100)->reachedDestination()) {
			state = EImplode1;
			gather(Coordinate(rand() % 800, rand() % 800));
		}
	}
	else if (state == EImplode1) {
		if(allObjects[allObjects.size() / 2] ->reachedDestination()) {
			state = ESolve2;
			solveAll();
		}
	}

	else if (state == ESolve2) {
		if(allObjects[allObjects.size() / 2]->reachedDestination()) {
			state = EHalfImplode1;
			gather(Coordinate(rand() % 800, rand() % 800));
		}
	}

	else if (state == EHalfImplode1) {
		//parts of the code won't survive simple changes, like the sizes
		if ( allObjects[allObjects.size() / 2 + 20] -> reachedDestination() ) {
			state = EMoveTogether1;
			randomTogether();
			timer.start();
		}
	}

	else if (state == EMoveTogether1) {
		if (timer.elapsed() >= 5 * secondDivisions) {
			state = EShakeTogether1;
			randomTogether();
			timer.restart();
		}
	}

	else if (state == EShakeTogether1) {
		randomTogether();
		if(timer.elapsed() >= 5 * secondDivisions) {
			state = EMix1;
			makeRandom();
		}
	}

	else if (state == EMix1) {
		if(allObjects.first()->reachedDestination()) {
			state = ESolve3;
			solveAll();
		}
	}

	else if (state == ESolve3) {
		if (allObjects[allObjects.size() / 2]->reachedDestination()) {
			state = EExplode1;
			random();
			timer.restart();
		}
	}

	else if (state == EExplode1) {
		if (timer.elapsed() >= 5 * secondDivisions) {
			randomTogether();
			if (allObjects.first()->getSpeedModule() > 20) {
				makeRandom();
				state = EMix2;
			}
		}
	}

	else if (state == EMix2) {
		makeRandom();
		if(timer.elapsed() >= 5 * secondDivisions) {
			state = ESolve4;
			solveAll();
		}
	}

	else if (state == ESolve4) {
		if(allObjects.first()->reachedDestination()) {
			state = EExplode2;
			timer.restart();
		}
	}

	else if (state == EExplode2) {
		random();
		if (timer.elapsed() >= 5 * secondDivisions) {
			state = ESolve5;
			randomTogether();
			solveAll();
		}
	}
	else if (state == ESolve5) {
		//this time check that the image is really composed
		bool composed = true;
		int i = 0;
		while(composed && i < allObjects.size()) {
			if (! allObjects.at(i)->reachedDestination())
				composed = false;
			i++;
		}
		if (composed) {
			state = ESlowlyDecompose1;
			slowlyDecomposeCounter = 0;
		}
		//check for full composition only the first time
		else if (episode > 0) {
			if (allObjects.first()->reachedDestination()) {
				//stop the objects from coming back, if an objects did not reach its destination it won't at all
				for(int i = 0; i < allObjects.size(); i++)
					if (! allObjects.at(i)->reachedDestination())
						allObjects.at(i)->unsetDestination();

				state = ESlowlyDecompose1;
				slowlyDecomposeCounter = 0;
			}
		}
	}
	else if (state == ESlowlyDecompose1) {
		if (slowlyDecomposeCounter < allObjects.size()) {
			//percents here
			int nrTake;
			if (slowlyDecomposeCounter < 5.0 / 100.0 * allObjects.size())
				nrTake = nrCol / 2;
			else if (slowlyDecomposeCounter < 10.0 / 100.0 * allObjects.size())
				nrTake = 1;
			else if (slowlyDecomposeCounter < 98.0/100.0 * allObjects.size())
				nrTake = nrCol / 2;
			else
				nrTake = 1;

			//take care here, the program can crash if it does not check for limits
			for (int i = 0; i < nrTake; i++) {
				allObjects[slowlyDecomposeCounter]->setSpeed(Speed(-50, -50));
				slowlyDecomposeCounter++;
			}
		}
		else {
			state = EImplode2;
			gather(Coordinate(rand() % 800, rand() % 800));
			episode++;
		}
	}

	else if(state == EImplode2) {
		if (allObjects.first()->reachedDestination()) {
			state = EExplode3;
			random();
			timer.restart();
		}
	}

	else if (state == EExplode3) {
		if (timer.elapsed() >= 10 * secondDivisions) {
			timer.restart();
			state = EFollowing1;
			Speed x(rand() % 1000 + 300, rand() % 1000 + 300);
			allObjects.first()->setSpeed(x);
			for (int i = 1; i < allObjects.size(); i++) {
				allObjects.at(i)->setSpeed(x);
				allObjects.at(i)->setSpeedModule( x.speedModule() / i * 50  + 30 );
				allObjects.at(i)->setDestination(allObjects.at(i-1));
			}
		}
	}

	else if(state == EFollowing1) {
		if (timer.elapsed() >= 15 * secondDivisions) {
			state = EGoingCenter1;
			gather(Coordinate(rand() % 800, rand() % 800));
		}
	}


	else {
		qDebug() << "PuzzleWorldShow, no such state" << state;
		state = EGoingCenter1;
	}

	World::step(seconds);
}

QString PuzzleWorldShow::getName(){
	return QString("PuzzleWorldShow");
}

QString PuzzleWorldShow::getDescription() {
	return QString("This one does spectacular animations by itself.\nYou can even interfere with its actions.\nIf your interference blocks it into a state, press \"Move randomly together\" and \"Solve\"");
}
