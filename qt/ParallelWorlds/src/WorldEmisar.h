#ifndef WORLDEMISAR_H
#define WORLDEMISAR_H

#include <QString>

class WorldControlInterface;
class World;

//small class for communicating with the GUI
class WorldEmisar {
public:
	WorldEmisar(WorldControlInterface* nInterface, QString nName = "DefaultName", QString nDescription = "Not initialised");
	QString getName() const;
	QString getDescription() const;
	bool makeInstance() const;
// 	(World* (* constrPointer)()) getConstructor() const;
// 	World* (*getConstructor)() const; //getConstructor() const;
// 	void* getConstructor() const;
private:
	WorldControlInterface* interface;
	QString name;
	QString description;
// 	void* constrPointer; //pointer to a function, the constructor
// 	World* (*constrPointer)();
};


#endif
