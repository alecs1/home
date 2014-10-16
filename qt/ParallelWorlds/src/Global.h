#ifndef GLOBAL_H
#define GLOBAL_H

//settings section
//use it to pass settings changes, in order to avoid creating so many functions
//could make the work on bits, for space improvements
class DrawSettings {
public:
	DrawSettings() {
		autoFillBg = true;
	}
	bool autoFillBg;
	bool autoCleanBg;

};

#endif
