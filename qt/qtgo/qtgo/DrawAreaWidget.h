#ifndef DRAWAREAWIDGET_H
#define DRAWAREAWIDGET_H

#include <QWidget>

class GoTable;

class DrawAreaWidget : public QWidget
{
    Q_OBJECT
public:
    explicit DrawAreaWidget(QWidget *parent = 0);
    void setChildTable(GoTable* aTable);

    void paintEvent(QPaintEvent *);
signals:

public slots:

private:
    GoTable* table;
};

#endif // DRAWAREAWIDGET_H
