#pragma once

#include <QWidget>

#include "Global.h"
#include "Settings.h"

class GoTableWidget;

/**
 * The area around the grid, containing the row and column numbering.
 */
class DrawAreaWidget : public QWidget
{
    Q_OBJECT
public:
    explicit DrawAreaWidget(QWidget *parent = 0);
    void setChildTable(GoTableWidget *aTable);

    void paintEvent(QPaintEvent *);
    void resizeEvent(QResizeEvent* event);
signals:

public slots:
    void changeProgramSettings();
    void changeGameSettings(SGameSettings newSettings);
    void changeHighlight(int row, int col);

private:
    QSize computeMinSize();
    void updateSizes();

private:
    GoTableWidget* tableW = nullptr;
    //for now we duplicate stuff from GoTable
    SGameSettings* gameSettings = nullptr;
    SProgramSettings* programSettings = nullptr;
    bool showBottomAndRightSymbols = false;
    int tableSize;
    int highlightRow = -1, highlightCol = -1;
    float leftMargin, rightMargin, topMargin, bottomMargin;
    float hOffset, vOffset;
    float tablePrivateSize;
    int textPointSize;
};
