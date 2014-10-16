#include "WorldEmisar.h"
#include "WorldControlInterface.h"

WorldEmisar::WorldEmisar(WorldControlInterface* nInterface, QString nName, QString nDescription) : interface(nInterface), name(nName), description(nDescription) {

}

QString WorldEmisar::getName() const {
	return name;
}

QString WorldEmisar::getDescription() const {
	return description;
}

bool WorldEmisar::makeInstance() const {
	return interface->addWorld(name);
}
/*
void* WorldEmisar::getConstructor() const {
	return constrPointer;
}*/


//end
