#include <QPainter>

#include "GoTable.h"
#include "DrawAreaWidget.h"

DrawAreaWidget::DrawAreaWidget(QWidget *parent) : QWidget(parent)
{
    programSettings = Settings::getProgramSettings();
}

void DrawAreaWidget::setChildTable(GoTable *aTable) {
    table = aTable;
    setMinimumSize(computeMinSize());
}

void DrawAreaWidget::paintEvent(QPaintEvent *) {
    //background
    QPainter painter(this);
    QColor background(programSettings->tableColour);
    painter.fillRect(QRectF(0, 0, width(), height()), background);
}

/*
 * @return - min size of the table plus our own min size for drawing the fonts
 */
QSize DrawAreaWidget::computeMinSize() {
    Q_ASSERT(table);
    int w = table->minimumSize().width();
    int h = table->minimumSize().height();
    return QSize(w, h);
}


void DrawAreaWidget::resizeEvent(QResizeEvent* event) {
    Q_UNUSED(event);
    if ( !(table && gameSettings)) {
        printf("%s - it's to early to do resizing, we don't have table and settings yet' :)\n", __func__);
        return;
    }

    int columnCountEquiv = gameSettings->size + 1;
    if (showBottomAndRightSymbols)
        columnCountEquiv += 1;
    float dist = GoTable::gridDist(height(), columnCountEquiv);
    float diameter = GoTable::stoneDiameter(dist);


    //width of column symbols is bound by size of a stone
    //height of row symbols is bound by size of a stone

}

