#ifndef LISTERINTERFACE_H
#define LISTERINTERFACE_H

#include <QString>

class WorldEmisar;
class WorldBuddy;


//an abstract class. Interface that some widget/other thing showing the worlds must implement
class ListerInterface {
public:
// 	virtual ~ListerInterface();
	virtual void addEmisar(WorldEmisar* newEmisar) = 0;
	virtual void removeEmisar(WorldEmisar* emisar) = 0;
	virtual void addBuddy(WorldBuddy* buddy) = 0;
	virtual void removeBuddy(QString buddyName) = 0;
	//I get a warning that the class has no virtual destructor, see what about this
};


#endif
