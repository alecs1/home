#include "MainDock.h"

#include "WorldEmisar.h"
#include "WorldBuddy.h"
#include "WorldEmisarWidget.h"
#include "WIBuddyWidget.h"

#include <QtDebug>
#include <QVBoxLayout>
#include <QMenuBar>
#include <QScrollArea>
#include <QMessageBox>
#include <QSplitter>

//pd Qt version dependent
#if QT_VERSION >= 0x040200
	#include <QDesktopServices>
	#include <QUrl>
#else
	#include <QFile>
	#include <QTextStream>
	#include <QTextEdit>
#endif

MainDock::MainDock(QWidget* parent) : QDockWidget("Control dock", parent) {

	settings = new DrawSettings;

	setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable );
	setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea);

	widget = new QWidget;
	layout = new QVBoxLayout;
	widget->setLayout(layout);
	setWidget(widget);


	createMenu();
	layout->addWidget(menuBar);

	splitter = new QSplitter;
	splitter->setOrientation(Qt::Vertical);
	layout->addWidget(splitter);


	//creating the list of widgets
	listWidget = new QWidget(this);
	scrollArea = new QScrollArea(this);
	listWidgetLayout = new QVBoxLayout(listWidget);
	listWidget->setLayout(listWidgetLayout);
	scrollArea->setWidget(listWidget);
	scrollArea->setWidgetResizable(true);
	splitter->insertWidget(0, scrollArea);

	ILWidget = new QWidget(this);
	ILScrollArea = new QScrollArea(this);
	ILLayout = new QVBoxLayout(ILWidget);
	ILWidget->setLayout(ILLayout);
	ILScrollArea->setWidget(ILWidget);
	ILScrollArea->setWidgetResizable(true);
	splitter->insertWidget(1, ILScrollArea);

	//nasty, set the widget type, then call the function to toggle it and set anything necessary
	widgetType = 1;
	toggleWidgetType();

	//problems with the size here
	ILWidget->setMinimumSize(300, 300);

// 	//mem leaks test:
// 	WorldEmisar temp;
// 	for( int i = 0; ;i++ ) {
// 		WorldEmisarWidget* xxx = new WorldEmisarWidget(&temp);
// 		emisarList << xxx;
// 		delete emisarList.takeAt(0);
// 		if (i % 200 == 0)
// 			qDebug() << i;
// 	}

	showComponents = true;
}

void MainDock::createMenu() {
	menuBar = new QMenuBar(this);

	fileMenu = menuBar->addMenu(tr("&File"));
	settingsMenu = menuBar->addMenu(tr("&Settings"));
	helpMenu = menuBar->addMenu(tr("&Help"));

	createActions();

	fileMenu->addAction(quitAction);
	settingsMenu->addAction(toggleWidgetTypeAction);
	settingsMenu->addAction(toggleFrameAction);
	settingsMenu->addAction(toggleComponentsAction);
	settingsMenu->addAction(toggleAutoFillBgAction);
	settingsMenu->addAction(toggleAutoCleanBgAction);
	helpMenu->addAction(helpAction);
	helpMenu->addAction(aboutAction);
}

void MainDock::createActions() {
	quitAction = new QAction(tr("Quit"), this);
	quitAction->setShortcut(tr("Ctrl+Q"));
	connect(quitAction, SIGNAL(triggered()), this, SIGNAL(quit()));

	toggleFrameAction = new QAction(tr("Show/Hide frame"), this);
	connect(toggleFrameAction, SIGNAL(triggered()), this, SIGNAL(toggleFrame()));

	toggleComponentsAction = new QAction(tr("Show/Hide dock"), this);
	connect(toggleComponentsAction, SIGNAL(triggered()), this, SLOT(toggleShowComponents()));

	toggleWidgetTypeAction = new QAction(tr("Swith widget type"), this);
	connect(toggleWidgetTypeAction, SIGNAL(triggered()), this, SLOT(toggleWidgetType()));

	toggleAutoFillBgAction = new QAction(tr("No fill background"), this);
	connect(toggleAutoFillBgAction, SIGNAL(triggered()), this, SLOT(toggleAutoFillBg()));

	helpAction = new QAction(tr("Help"), this);
	helpAction->setShortcut(tr("F1"));
	connect(helpAction, SIGNAL(triggered()), this, SLOT(help()));

	aboutAction = new QAction(tr("About"), this);
	connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));

	toggleAutoCleanBgAction = new QAction(tr("No clean background"), this);
	connect(toggleAutoCleanBgAction, SIGNAL(triggered()), this, SLOT(toggleCleanBg()));
}

void MainDock::about() {
	QString aboutParallelWorlds;
	aboutParallelWorlds = QString::fromUtf8("Parallel Worlds 0.56\n\nThe Parallel Worlds space simulator.\n\n© 2008, Alex Dănilă\nalex.danila@gmail.com\nThis program is distributed under the terms of GPL v2 or later.");
	QMessageBox::about(this,
	                   tr("About Parallel Worlds"),
	                   aboutParallelWorlds);
}

void MainDock::help() {
//pd Qt version dependent
#if QT_VERSION >= 0x040200
	QDesktopServices::openUrl(QUrl("Readme.txt"));
#else
	QFile readmeFile("Readme.txt");
	readmeFile.open(QIODevice::ReadOnly);
	QTextStream readmeStream(&readmeFile);
	QString readmeString = readmeStream.readAll();
	//won't close automaticaly
	QTextEdit* readmeTXE = new QTextEdit(0);
	readmeTXE->setMinimumSize(500, 300);
	readmeTXE->setWindowTitle("Parallel Worlds Help");
	readmeTXE->show();
	readmeTXE->setReadOnly(true);
	readmeTXE->setPlainText(readmeString);
#endif
}

//ListerInterface
void MainDock::addEmisar(WorldEmisar* newEmisar) {
	//search for its duplicate, and only then insert the new one
	removeEmisar(newEmisar);

	WorldEmisarWidget* newWidget = new WorldEmisarWidget(newEmisar);
	emisarList << newWidget;
	listWidgetLayout->addWidget(newWidget);

	//this is only done to djust the width of dock, prob
	scrollArea->setMinimumSize(250, 300);
}

void MainDock::removeEmisar(WorldEmisar* theEmisar) {
	int i = 0;
	while( i < emisarList.size() ) {
		if(emisarList[i]->name() == theEmisar->getName())
			delete emisarList.takeAt(i); // no need to advance
		else
			i++;
	}
	listWidget->adjustSize();
// 	listWidget->setMinimumSize(listWidget->sizeHint());
}

void MainDock::addBuddy(WorldBuddy* buddy) {
	WIBuddyWidget* nWidget = new WIBuddyWidget(buddy);
	WIList << nWidget;
	ILLayout->addWidget(nWidget);
	ILScrollArea->setMinimumSize(ILWidget->sizeHint().width(), 100);
	ILWidget->setMinimumSize(ILWidget->sizeHint());
}

void MainDock::removeBuddy(QString buddyName) {
	int i = 0;
	while( i < WIList.size() ) {
		if(WIList[i]->name() == buddyName) {
			delete WIList.takeAt(i);
		}
		else {
			i++;
		}
	}
	ILWidget->setMinimumSize(ILWidget->sizeHint());
}

void MainDock::toggleShowComponents() {
	if (showComponents) {
		showComponents = false;
		splitter->hide();
		setFloating(true);
		adjustSize();
	}
	else {
		showComponents = true;
		splitter->show();
	}
}

void MainDock::toggleWidgetType() {
	if (widgetType == 0) {
		widgetType = 1;
		toggleWidgetTypeAction->setText(tr("Use QWidget"));
		emit setWidgetType(widgetType);
	}

	else if (widgetType == 1) {
		widgetType = 0;
		toggleWidgetTypeAction->setText(tr("Use QGLWidget"));
		emit setWidgetType(widgetType);
	}
	//should also call the settingsChanged
	//for the moment just reset the settings:
	settings->autoFillBg = false;
	toggleAutoFillBgAction->setText(tr("Auto fill background"));
}

void MainDock::toggleAutoFillBg() {
	if (settings->autoFillBg) {
		settings->autoFillBg = false;
		toggleAutoFillBgAction->setText(tr("Auto fill background"));
	}

	else {
		settings->autoFillBg = true;
		toggleAutoFillBgAction->setText(tr("No fill background"));
	}
// 	qDebug() << "MainDock::toggleAutoFillBackground()";
	emit drawSettingsChanged(settings);
}

void MainDock::toggleCleanBg() {
	if (settings->autoCleanBg) {
		settings->autoCleanBg = false;
		toggleAutoCleanBgAction->setText(tr("Auto clean background"));
	}
	else {
		settings->autoCleanBg = true;
		toggleAutoCleanBgAction->setText(tr("No clean background"));
	}
	emit drawSettingsChanged(settings);
}

//end
