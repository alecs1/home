#ifndef MAINDOCK_H
#define MAINDOCK_H

#include <QDockWidget>
#include "ListerInterface.h"
#include "Global.h"

class QMenuBar;
class QVBoxLayout;
class QMenu;
class QSplitter;
// class QHBoxLayout;
// class QLabel;
// class QToolButton;
class QScrollArea;
class WorldEmisar;
class WorldBuddy;
class WorldEmisarWidget;
class WIBuddyWidget;


class MainDock : public QDockWidget, public ListerInterface {
	Q_OBJECT
public:
	MainDock(QWidget* parent);
// 	~MainDock();
	//ListerInterface:
	void addEmisar(WorldEmisar* newEmisar);
	void removeEmisar(WorldEmisar* theEmisar);
	void addBuddy(WorldBuddy* buddy);
	void removeBuddy(QString buddyName);

public slots:
	void help();
	void about();
	void toggleShowComponents();
	void toggleWidgetType();
	void toggleAutoFillBg();
	void toggleCleanBg();

signals:
	void quit();
	void toggleFrame();
	void setWidgetType(int widgetType);
	void drawSettingsChanged(DrawSettings* newSettings);


private:
	//the menu
	QMenuBar* menuBar;
	QMenu* fileMenu;
	QMenu* settingsMenu;
	QMenu* helpMenu;

	//the actions
	QAction* quitAction;
	QAction* helpAction;
	QAction* aboutAction;
	QAction* aboutQtAction;
	QAction* toggleFrameAction;
	QAction* toggleComponentsAction;
	QAction* toggleWidgetTypeAction;
	QAction* toggleAutoFillBgAction;
	QAction* toggleAutoCleanBgAction;

	int widgetType; //0 for QWidget, 1 for QGLwidget, use an enum for this

	bool showComponents;

	//these are candidates for creating a new widget, or to be replaced with some of the model/view stuff from Qt.
	//rename all this stuff
	QWidget* listWidget;
	QScrollArea* scrollArea;
	QVBoxLayout* listWidgetLayout;
	QList<WorldEmisarWidget*> emisarList;
	//IL = instances list
	QScrollArea* ILScrollArea;
	QWidget* ILWidget;
	QVBoxLayout* ILLayout;
	QList<WIBuddyWidget*> WIList;

	QWidget* widget;
	QSplitter* splitter;
	QVBoxLayout* layout;

	DrawSettings* settings;

private:
	void createMenu();
	void createActions();

};

#endif
