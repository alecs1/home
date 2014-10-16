#include "CWRect.h"
#include <QtDebug>

//Colisions World Rect
CWRect::CWRect(float nLife, float newW, float newH) : Rect(newW, newH) {
	if (nLife == 0) {
		imortal = true;
	}
	else {
		totalLife = nLife;
		life = totalLife;
		imortal = false;
		dead = false;
	}
}

void CWRect::decreaseLife(float value) {
	if (imortal) {
		qDebug() << "Warning, decreasing life of an imortal, nothing was done";
		return;
	}

	life -= value;

	if (life <= 0) {
		if (! dead) {
			dead = true;
			//			qDebug() << "Object died";
		}
		return;
	}

	updateColor();
}

void CWRect::updateColor() {
	//the object starts pure green and goes darker
	int g = color.green();
	color.setRgb(0, life / totalLife * 255, 0);
}

bool CWRect::isDead() const {
	return dead;
}

//end
