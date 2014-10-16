#ifndef WORLDEMISARWIDGET_H
#define WORLDEMISARWIDGET_H

#include <QWidget>

class QHBoxLayout;
class QLabel;
class WorldEmisar;
class QToolButton;

class WorldEmisarWidget : public QWidget {
	Q_OBJECT
public:
	WorldEmisarWidget(WorldEmisar* nEmisar);
	QString name() const; //try: replace all instances of getProperty() with property()
// 	~WorldEmisarWidget();
private slots:
	void makeInstance(); //make it private for the moment

private:
	QHBoxLayout* layout;
	WorldEmisar* emisar;
	QLabel* nameLabel;
	QToolButton* createButton;
};

#endif
