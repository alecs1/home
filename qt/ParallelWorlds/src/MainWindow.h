#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "Global.h"

class MainDock;
class UniverseWidgetBase;
// class UniverseWidgetGL;
class Universe;
class QGridLayout;
class QFrame;

class MainWindow : public QMainWindow {
	Q_OBJECT
public:
	MainWindow();
	~MainWindow();

protected:
	void mouseReleaseEvent(QMouseEvent* event);
	void closeEvent(QCloseEvent* event);

private slots:
	void toggleShowFrame();
	void setWidgetType(int widgetType);
	void drawSettingsChanged(DrawSettings* newSettings);

private:
	bool showFrame;
	Universe* universe;
	UniverseWidgetBase* universeW;
// 	UniverseWidgetGL* universeWGL;
	QGridLayout* layout;
	QFrame* frame;
	MainDock* mainDock;
	QIcon* icon;
	int crtWidgetType;

};

#endif
