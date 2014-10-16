#ifndef WIBUDDYWIDGET_H
#define WIBUDDYWIDGET_H

#include <QWidget>
class WorldBuddy;
class QHBoxLayout;
class QLabel;
class QToolButton;

//World Instance Buddy Widget
class WIBuddyWidget : public QWidget {
	Q_OBJECT
public:
	WIBuddyWidget(WorldBuddy* nBuddy);
	QString name() const;
private slots:
	void deleteWorld();

private:
	WorldBuddy* buddy;
	QHBoxLayout* layout;
	QLabel* nameLabel; //should depend upon the count of the worlds
	QToolButton* showControlsButton;
	QToolButton* deleteButton;
};

#endif
