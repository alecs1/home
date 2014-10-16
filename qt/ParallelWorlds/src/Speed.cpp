#include "Speed.h"
#include "Vector.h"

#include <QtDebug>

#include <cmath>

Speed::Speed(float newX, float newY, float newZ) {
	x = newX;
	y = newY;
	z = newZ;
}

float Speed::speedModule() const {
	return sqrt( x*x + y*y + z*z);
}

//the function keeps the speed module (modul, valoare absolută în Română) but changes its direction, or vector
void Speed::setVectorDirection(Vector newVector) {
	/*this is more like math, but it says that  the speed should be the same, while the direction changes
	we receive a direction vector, for that direction must apply some speed, meaning we will multiply the
	direction values with one other value (named it c) that will give speed
	That values should be calculated like this: c = absoluteSpeed(crtSpeed) / absoluteValue(vector)
	Hope it works :)
	*/
	//raport, nu-mi vine ]n mine cum se spune ]n englez[
	//avoid infinity by altering one of the values if the vector is null, this is only a workaround, remember to implement the real soluition described in the header file
	Vector crtVector = newVector;
	if (crtVector.isNull()) {
		crtVector = Vector(0.001, 0, 0);
	}

	float raport = speedModule() / crtVector.absoluteValue();
// 	qDebug() << "speed: " << x << y << z << " = " << speedModule();
// 	qDebug() << "raport: " << speedModule() << "/" << crtVector.absoluteValue() << " = " << raport;
	x = crtVector.x * raport;
	y = crtVector.y * raport;
	z = crtVector.z * raport;
// 	qDebug() << "the new vector: " << newVector.x << newVector.y << newVector.z;
// 	qDebug() << "new speed: " << x << y << z << " = " << speedModule();
}

void Speed::setModule(float newModule) {
	float raport = newModule / speedModule();
	x = x * raport;
	y = y * raport;
	z = z * raport;
}
