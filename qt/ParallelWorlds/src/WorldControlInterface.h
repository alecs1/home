#ifndef WORLDCONTROLINTERFACE_H
#define WORLDCONTROLINTERFACE_H

#include <QString>

class WorldControlInterface {
public:
// 	virtual ~WorldControlInterface();
	virtual bool addWorld(QString worldName) = 0;
	virtual bool removeWorld(QString worldName) = 0;
};

#endif
