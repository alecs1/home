#ifndef UNIVERSEWIDGETBASE_H
#define UNIVERSEWIDGETBASE_H

#include <QWidget>
#include "Global.h"

class ListerInterface;
class Universe;


//2014-10-16 - this is some multiple inheritance OOP bullshit I had in mind at that time
//all UniverseWidget classes should inherit this using virtual inheritance
//we need to inherit QWidget
class UniverseWidgetBase : public QWidget {
	Q_OBJECT
public:
	virtual void setListerInterface(ListerInterface* newLister);
	virtual void setUniverse(Universe* aUniverse);

protected:
	virtual void paintEvent(QPaintEvent* event);
	virtual void mouseReleaseEvent(QMouseEvent* event);
	virtual void wheelEvent(QWheelEvent* event);
	virtual void resizeEvent(QResizeEvent* event);

protected:
    Universe* universe;
    QTimer* universeTimer;
    QTime* fpsTime;
    float seconds;
    float fps;
    bool fixedFpsMode;
    QPalette palette;
    QPixmap* pixmap;
    QRectF rect;

protected:
	virtual void createConnections();

protected slots:
	virtual void step();
	virtual void toggleFrameMode();

public slots:
	virtual void drawSettingsChanged(DrawSettings* newSettings);

};


#endif
