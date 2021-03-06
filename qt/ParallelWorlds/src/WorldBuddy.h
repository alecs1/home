#ifndef WORLDBUDDY_H
#define WORLDBUDDY_H

#include <QString>
class WorldControlInterface;
class World;

class WorldBuddy {
public:
	WorldBuddy(WorldControlInterface* interface, World* pointer, QString nName);
	QString getName() const;
	World* getWorldPointer() const;
	void deleteIt();

private:
    WorldControlInterface* interface;
	World* worldPointer;
    QString name;
};

#endif
