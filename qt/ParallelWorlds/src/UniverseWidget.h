#ifndef UNIVERSEWIDGET_H
#define UNIVERSEWIDGET_H

#include "UniverseWidgetBase.h"

#include <QWidget>

class ListerInterface;
class Universe;

class UniverseWidget : public UniverseWidgetBase {
	Q_OBJECT
public:
	UniverseWidget();
// 	void setListerInterface(ListerInterface* newLister);
// 	void setUniverse(Universe* aUniverse);

protected:
// 	void paintEvent(QPaintEvent* event);
// 	void mouseReleaseEvent(QMouseEvent* event);
// 	void wheelEvent(QWheelEvent* event);
// 	void resizeEvent(QResizeEvent* event);

private:
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
public slots:
	void drawSettingsChanged(DrawSettings* newSettings);

};


#endif
