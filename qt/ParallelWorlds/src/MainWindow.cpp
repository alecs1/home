#include "MainWindow.h"
#include "UniverseWidgetBase.h"
#include "UniverseWidget.h"
#include "UniverseWidgetGL.h"
#include "Universe.h"
#include "MainDock.h"

#include <QFrame>
#include <QGridLayout>
#include <QtDebug>
#include <QMouseEvent>

MainWindow::MainWindow() {
	setWindowTitle("Parallel Worlds");


	//pd
	#if QT_VERSION >= 0x040200
		icon = new QIcon("images/icons/parallel worlds.svg");
	#else
		icon = new QIcon("images/icons/parallel worlds.png");
	#endif

	setWindowIcon(*icon);

	universeW = NULL;
// 	universeWGL = NULL;

	universe = new Universe;
	universeW = new UniverseWidget;
	universeW->setUniverse(universe);
	crtWidgetType = 0;

	mainDock = new MainDock(this);
	addDockWidget(Qt::LeftDockWidgetArea, mainDock);
	connect(mainDock, SIGNAL(quit()), this, SLOT(close()));
	connect(mainDock, SIGNAL(toggleFrame()), this, SLOT(toggleShowFrame()));
	connect(mainDock, SIGNAL(setWidgetType(int)), this, SLOT(setWidgetType(int)));
	connect(mainDock, SIGNAL(drawSettingsChanged(DrawSettings*)), this, SLOT(drawSettingsChanged(DrawSettings*)));

	universeW->setListerInterface(mainDock);

	setCentralWidget(universeW);
	showFrame = false;

	frame = new QFrame;
	layout = new QGridLayout(frame);

	universe->addWorld("BeesWorld");
	//setAttribute(Qt::WA_DeleteOnClose); //makes double delete -> crash
}

MainWindow::~ MainWindow() {
	//some of them are already children of the mainWindow

	//the universeW might have been closed already by closeEvent
	if (universe != NULL)
		delete universe;
	if (universeW != NULL)
 		delete universeW;
// 	if (universeWGL != NULL)
// 		delete universeWGL;
 	delete layout;
// 	delete frame;
	delete mainDock;
	qDebug() << "~MainWindow";
}

void MainWindow::mouseReleaseEvent(QMouseEvent* event) {
	if(event->button() == Qt::RightButton) {
		if(windowState() == Qt::WindowFullScreen)
			setWindowState(Qt::WindowNoState);
		else
			setWindowState(Qt::WindowFullScreen);
	}
}

void MainWindow::closeEvent(QCloseEvent* event) {
	//have the program close at a close event
	//not testing for deletion, as I see no way that a closeEvent could come after they have been deleted
	delete universeW;
	universeW = NULL;

	delete universe;
	universe = NULL;

// 	if (universeWGL != NULL) {
// 		delete universeWGL;
// 		universeWGL = NULL;
// 	}

	qDebug() << "MainWindow::closeEvent";
}

void MainWindow::toggleShowFrame() {
	if (showFrame) {
		//give up the frame children, take universeW as child
		showFrame = false;
		frame->setParent(0);
		setCentralWidget(universeW);
		//this shit gives me a lot of problems on Linux/X11, the performance becomes shit instantly
		//something connected to the signals and slots and the timer, thread might solve the problem
	}
	else {
		//give up universeW children, take frame as child
		showFrame = true;
		universeW->setParent(0);
		layout->addWidget(universeW);

		setCentralWidget(frame);
	}
}

void MainWindow::setWidgetType(int widgetType) {
	//a quite costly operation, make a list of widgets and keep them around
	if (crtWidgetType == widgetType)
		return;
	delete universeW;
	if (widgetType == 0)
		universeW = new UniverseWidget;
	else if (widgetType == 1)
		//this code is begging for a crash :)
		universeW = dynamic_cast <UniverseWidgetBase*> (new UniverseWidgetGL);

	crtWidgetType = widgetType;

	universeW->setUniverse(universe);
	universeW->setListerInterface(mainDock);
	setCentralWidget(universeW);
// 	emit drawSettingsChanged(settings);
}

void MainWindow::drawSettingsChanged(DrawSettings* newSettings) {
	qDebug() << "MainWindow::drawSettingsChanged())";
	universeW->drawSettingsChanged(newSettings);
}

//end
