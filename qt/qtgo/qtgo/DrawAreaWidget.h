#ifndef DRAWAREAWIDGET_H
#define DRAWAREAWIDGET_H

#include <QWidget>

#include "Global.h"
#include "Settings.h"

class GoTable;

class DrawAreaWidget : public QWidget
{
    Q_OBJECT
public:
    explicit DrawAreaWidget(QWidget *parent = 0);
    void setChildTable(GoTable* aTable);

    void paintEvent(QPaintEvent *);
    void resizeEvent(QResizeEvent* event);
signals:

public slots:

private:
    QSize computeMinSize();


private:
    GoTable* table = NULL;
    //for now we duplicate stuff from GoTable
    SGameSettings* gameSettings = NULL;
    SProgramSettings* programSettings = NULL;
    bool showBottomAndRightSymbols = true;
    int tableSize;
    int hOffset, vOffset;
    int textPointSize;
};

#endif // DRAWAREAWIDGET_H
