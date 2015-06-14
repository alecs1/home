#include <QPainter>

#include "GoTable.h"
#include "DrawAreaWidget.h"
#include "Utils.h"

QList<QString> rowNumbering { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19" };
QList<QString> colNumbering { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T" };

QString defaultFont = "Liberation Sans";


DrawAreaWidget::DrawAreaWidget(QWidget *parent) : QWidget(parent)
{
    programSettings = Settings::getProgramSettings();
    gameSettings = new SGameSettings;
    gameSettings->size = 19;
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

    QFont font(defaultFont, textPointSize);
    QFontMetrics fontMetrics = QFontMetrics(font);
    float textHeight = fontMetrics.ascent();
    float dist = GoTable::gridDist(tableSize, gameSettings->size);
    float marginSpace = dist;
    float diameter = GoTable::stoneDiameter(dist);
    painter.setFont(font);
    for (int i = 0 ; i < gameSettings->size; i++) {
        QString text = rowNumbering[gameSettings->size-i-1];
        int textWidth = fontMetrics.width(text);
        qreal yPos = dist + i * dist - textHeight/2 + vOffset;
        QRectF leftRect((marginSpace - textWidth)/2, yPos, textWidth, textHeight);
        QRectF rightRect(dist * gameSettings->size + diameter/2 + (marginSpace - textWidth)/2 + hOffset, yPos, textWidth, textHeight);
        painter.drawText(leftRect, Qt::AlignCenter, text);
        painter.drawText(rightRect, Qt::AlignCenter, text);
    }

    painter.setFont(font);
    for (int i = 0; i < gameSettings->size; i++) {
        QString text = colNumbering[i];
        int textWidth = fontMetrics.width(text);
        qreal xPos = dist + i * dist - textWidth / 2 + hOffset;
        QRectF topRect(xPos, (marginSpace - textHeight) / 2, textWidth, textHeight);
        //TODO - this mat formula got out of hand
        QRectF bottomRect(xPos + hOffset, dist * gameSettings->size + diameter/2 + (marginSpace - textHeight)/2 + vOffset, textWidth, textHeight);
        painter.drawText(topRect, Qt::AlignCenter, text);
        painter.drawText(bottomRect, Qt::AlignCenter, text);
    }


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

    int columnCountEquiv = gameSettings->size + 2;
    if (showBottomAndRightSymbols)
        columnCountEquiv += 1;
    float dist = GoTable::gridDist(height(), columnCountEquiv);
    tableSize = (gameSettings->size + 1) * dist;
    table->resize(tableSize, tableSize);
    float diameter = GoTable::stoneDiameter(dist);
    hOffset = vOffset = diameter/2;
    table->move(hOffset, vOffset);
    //width of column symbols is bound by size of a stone
    //height of row symbols is bound by size of a stone
    int auxSmaller, auxLarger;
    Utils::PointSizeParams p;
    p.fontName = defaultFont;
    p.targetSize = diameter;
    p.measure = Utils::PointSizeParams::Measure::heightAscent;
    p.nextSmaller = &auxSmaller;
    p.nextLarger = &auxLarger;
    p.text = rowNumbering[0];
    textPointSize = Utils::getClosestPointSize(p);
}

