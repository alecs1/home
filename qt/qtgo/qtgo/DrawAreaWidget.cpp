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
    setMinimumWidth(300);
    setMinimumHeight(300);
}

void DrawAreaWidget::setChildTable(GoTable *aTable) {
    table = aTable;
    delete gameSettings;
    //TODO - we actually need a signal when this changes
    gameSettings = table->getGameSettingsPointer();
    setMinimumSize(computeMinSize());
}

void DrawAreaWidget::paintEvent(QPaintEvent *) {
    //background
    QPainter painter(this);
    QColor background(programSettings->tableColour);
    painter.fillRect(QRectF(0, 0, width(), height()), background);

    QFont font(defaultFont, textPointSize);
    QFontMetrics fontMetrics = QFontMetrics(font);
    float textHeight = fontMetrics.height();
    float dist = GoTable::gridDist(tableSize, gameSettings->size);
    float diameter = dist * GoTable::stoneDiameter();
    painter.setFont(font);
    //left and right
    for (int i = 0 ; i < gameSettings->size; i++) {
        QString text = rowNumbering[gameSettings->size-i-1];
        int textWidth = fontMetrics.width(text);
        qreal yPos = vOffset + dist + i * dist - textHeight/2;
        QRectF leftRect((leftMargin - textWidth)/2, yPos, textWidth, fontMetrics.height());
        painter.drawText(leftRect, Qt::AlignCenter, text);
        if (showBottomAndRightSymbols) {
            QRectF rightRect(leftMargin + tablePrivateSize + (rightMargin - textWidth)/2, yPos, textWidth, fontMetrics.height());
            painter.drawText(rightRect, Qt::AlignCenter, text);
        }
    }

    //top and bottom
    for (int i = 0; i < gameSettings->size; i++) {
        QString text = colNumbering[i];
        int textWidth = fontMetrics.width(text);
        QRect fontRect = fontMetrics.boundingRect(text);
        //printf("%s - fontRect: %d, %d, %d, %d\n", __func__, fontRect.x(), fontRect.y(),
        //       fontRect.width(), fontRect.height());
        qreal xPos = leftMargin + diameter/2 + i * dist - textWidth / 2;
        QRectF topRect(xPos, 0, textWidth, fontMetrics.height());
        painter.drawText(topRect, Qt::AlignCenter, text);
        if (showBottomAndRightSymbols) {
            QRectF bottomRect(xPos, topMargin + tablePrivateSize, textWidth, fontMetrics.height());
            painter.drawText(bottomRect, Qt::AlignCenter, text);
        }
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

    int boundingSize = width(); //compute and enforce correctly
    if (height() < boundingSize)
        boundingSize = height();

    //Steps (should transform in an iteration, since the formulae cannot be computed perfectly):
    //1. compute pessimistic size for the table and grid distance
    //2. based on the size decide for fonts size
    //3. based on the fonts decide the final table size.

    int columnCountEquiv = gameSettings->size + 1;
    if (showBottomAndRightSymbols)
        columnCountEquiv += 1;
    float dist = GoTable::gridDist(boundingSize, columnCountEquiv);
    float diameter = dist * GoTable::stoneDiameter();

    //top line (bottom line is different because it will have the descent cut out)
    int auxSmaller, auxLarger;
    Utils::PointSizeParams p;
    p.fontName = defaultFont;
    p.targetSize = diameter;
    p.measure = Utils::PointSizeParams::Measure::height;
    p.nextSmaller = &auxSmaller;
    p.nextLarger = &auxLarger;
    p.text = rowNumbering[0];
    textPointSize = Utils::getClosestPointSize(p);
    QFont font(p.fontName, textPointSize);
    QFontMetrics fontMetrics(font);
    topMargin = fontMetrics.ascent();
    bottomMargin = 0;
    if(showBottomAndRightSymbols)
        bottomMargin = fontMetrics.height();
    leftMargin = 0;
    foreach(QString str, colNumbering) {
        if (fontMetrics.width(str) > leftMargin)
            leftMargin = fontMetrics.width(str);
    }
    leftMargin = leftMargin;
    rightMargin = 0;
    if (showBottomAndRightSymbols)
        rightMargin = leftMargin;


    //at this point we've decided on the fonts, let's redistribute the tableSize
    //Table private drawing takes up: (gameSize - 1 + stoneDiameter) * dist
    //Total table size = (gameSize + 1) * dist

    int hSpace = width() - leftMargin - rightMargin;
    int vSpace = height() - topMargin - bottomMargin;
    boundingSize = hSpace;
    if (vSpace < hSpace)
        boundingSize = vSpace;
    dist = boundingSize / (gameSettings->size - 1 + GoTable::stoneDiameter());
    tablePrivateSize = (gameSettings->size - 1 + GoTable::stoneDiameter()) * dist;
    tableSize = (gameSettings->size + 1) * dist;
    diameter = GoTable::stoneDiameter() * dist;
    table->resize(tableSize, tableSize);
    vOffset = topMargin - dist + diameter/2;
    hOffset = leftMargin - dist + diameter/2;
    table->move(hOffset, vOffset);
}

