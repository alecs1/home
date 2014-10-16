#include "SinglePoint.h"

#include <QPainter>
#include <QPointF>
#include <QtDebug>

SinglePoint::SinglePoint() {
	color.setRgb(255, 0, 0);
}

void SinglePoint::draw(QPainter& painter) {
	drawQt(painter);
}

void SinglePoint::drawQt(QPainter& painter) {
	painter.drawPoint(QPointF(coord.x, coord.y));
}

void SinglePoint::testMarginColide() {

}
