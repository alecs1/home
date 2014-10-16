#ifndef MAIDENSUI_H
#define MAIDENSUI_H

#include "ui_maidens.h"

#include <QDockWidget>


class MaidensUi : public QWidget, public Ui::MaidensForm {
	Q_OBJECT
public:
	MaidensUi(QWidget* parent = 0);

private slots:
	void openFile();

signals:
	void fileChosen(QString);
	void back();
	void forward();
	void speedChanged(double);
	void start();
	void random();
};


#endif
