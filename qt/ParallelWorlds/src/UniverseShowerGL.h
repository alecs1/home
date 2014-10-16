#ifndef UNIVERSESHOWERGL_H
#define UNIVERSESHOWERGL_H

#include <QGLWidget>
#include "Global.h"

class Universe;

class UniverseShowerGL : public QGLWidget {
	Q_OBJECT
public:
	UniverseShowerGL();

// 	void paintStart();
	void setUniverse(Universe* aUniverse);

protected:
	void paintEvent(QPaintEvent* event);
	void initializeGL();
	void resizeEvent(QResizeEvent* event);

private:
	Universe* universe;
	QPixmap* pixmap;
	QPalette palette;
	QRectF rect;
	bool cleanBg;

public slots:
	void settingsChanged(DrawSettings* newSettings);


};

#endif
