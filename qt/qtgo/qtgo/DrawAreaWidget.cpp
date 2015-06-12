#include "DrawAreaWidget.h"

DrawAreaWidget::DrawAreaWidget(QWidget *parent) : QWidget(parent)
{

}

void DrawAreaWidget::setChildTable(GoTable *aTable) {
    table = aTable;
}

void GoTable::paintEvent(QPaintEvent *) {
    //background
    QColor background(programSettings.tableColour);
    if (players[crtPlayer] == PlayerType::AI && computing) {
        background = QColor(210, 200, 200);
    }
    painter.fillRect(QRectF(0, 0, width(), height()), background);
}
