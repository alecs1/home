#include "WorldBuddy.h"
#include "WorldControlInterface.h"

WorldBuddy::WorldBuddy(WorldControlInterface* nInterface, World* pointer, QString nName) : interface(nInterface), worldPointer(pointer), name(nName) {

}

QString WorldBuddy::getName() const {
	return name;
}

World* WorldBuddy::getWorldPointer() const{
	return worldPointer;
}


void WorldBuddy::deleteIt() {
	//the object will die before the function can complete, see if this works
	interface->removeWorld(name);
}

//end
