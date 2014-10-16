#ifndef CWRECT_H
#define CWRECT_H

#include "Rect.h"


//Colisions World Rect
class CWRect : public Rect {
public:
	CWRect(float nLife = 0, float newW = 50, float newH = 50);
	void decreaseLife(float value = 1);
	bool isDead() const;

protected:
	void updateColor();

private:
	bool dead;
	bool imortal;
	float totalLife;
	float life;
};

#endif
