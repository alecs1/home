#ifndef SINGLEPOINT_H
#define SINGLEPOINT_H

#include "PhysicalObject.h"

class SinglePoint : public PhysicalObject {
public:
	SinglePoint();
	void draw(QPainter& painter);

protected:
	void testMarginColide();

private:
	void drawQt(QPainter& painter);
};

#endif
