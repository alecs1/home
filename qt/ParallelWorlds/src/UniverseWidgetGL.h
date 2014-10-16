#ifndef UNIVERSEWIDGETGL_H
#define UNIVERSEWIDGETGL_H

#include <QGLWidget>

#include "UniverseWidgetBase.h"

class ListerInterface;
class Universe;
class UniverseShowerGL;

//copy paste from the original class, try to find a common part of the code then make a super class for these two
class UniverseWidgetGL : public UniverseWidgetBase {
	Q_OBJECT
public:
	UniverseWidgetGL();
// 	void setListerInterface(ListerInterface* newLister);
// 	void setUniverse(Universe* aUniverse);

protected:
	void paintEvent(QPaintEvent* event);
// 	void mouseReleaseEvent(QMouseEvent* event);
// 	void wheelEvent(QWheelEvent* event);
// 	void resizeEvent(QResizeEvent* event);

	//OpenGL functions now
protected:
	//the functionality will move to the paintEvent
// 	void paintGL();
// 	void initializeGL();
// 	void resizeGL(int width, int height);

private:
    UniverseShowerGL* GLChild;
//    Universe* universe;
//    QTimer* universeTimer;
//    QTime* fpsTime;
//    float seconds;
//    float fps;
//    bool fixedFpsMode;
//    QPalette palette;
//    QPixmap* pixmap;
//    QRectF rect;

private:
// 	void createConnections();

protected slots:
// 	void step();
// 	void toggleFrameMode();
	void drawSettingsChanged(DrawSettings* newSettings);

};


#endif
