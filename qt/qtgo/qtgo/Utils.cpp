#include <QFontMetrics>
#include <QScreen>
#include <QGuiApplication>

#include "Utils.h"

#include <limits>
//int Utils::getClosestPointSize(QString fontName, int targetSize, int& nextSmaller, int& nextLarger, int direction, QString text) {
int Utils::getClosestPointSize(PointSizeParams p) {
    //haha - clumsy c++ syntax :)))
    int closestSize = std::numeric_limits<int>::max();
    int nextSmallerSize = std::numeric_limits<int>::max();
    int nextLargerSize = std::numeric_limits<int>::max();
    int closest = -1;
    *p.nextSmaller = -1;
    *p.nextLarger = -1;
    int pointSize = 1;
    int maxPointSize = 96;
    QRect boundingRect;
    while(pointSize <= maxPointSize) {
        QFontMetrics fontMetrics = QFontMetrics(QFont(p.fontName, pointSize));
        int crtSize = 0;
        switch(p.measure) {
        case PointSizeParams::Measure::width:
            crtSize = fontMetrics.width(p.text);
            break;
        case PointSizeParams::Measure::heightAscent:
            crtSize = fontMetrics.ascent();
            break;
        case PointSizeParams::Measure::height:
            boundingRect = fontMetrics.boundingRect(p.text);
            crtSize = boundingRect.height();
            break;
        default:
            Q_ASSERT("this should not happen");
            break;
        }
        //printf("%s - pointSize=%d. Results in height=%d, we need height=%d\n",
        //       __func__, pointSize, crtSize, targetSize);
        if (abs(crtSize - p.targetSize) < abs(crtSize - closestSize)) {
            closest = pointSize;
            closestSize = crtSize;
        }
        if ( (crtSize < p.targetSize) && (abs(crtSize - p.targetSize) < abs(crtSize - nextSmallerSize)) ) {
            *p.nextSmaller = pointSize;
            nextSmallerSize = crtSize;
        }
        if ( (crtSize > p.targetSize) && (abs(crtSize - p.targetSize) < abs(crtSize - nextLargerSize)) ) {
            *p.nextLarger = pointSize;
            nextLargerSize = crtSize;
        }
        if ((closestSize != std::numeric_limits<int>::max()) &&
            (nextSmallerSize != std::numeric_limits<int>::max()) &&
            (nextLargerSize != std::numeric_limits<int>::max()))
            break;
        pointSize += 1;
    }
    //printf("%s, closest:%d:%d, smaller:%d:%d, larger:%d:%d\n", __func__,
    //       closest, closestSize, nextSmaller, nextSmallerSize, nextLarger, nextLargerSize);
    return closest;
}

//TODO - learn about DPI and point sizes and others; now is purely written by trial and error
int Utils::estimateQToolButtonSize() {
    const int MIN_SIZE = 15; //under 15 pixel should be an error
    const int PIXEL_FROM_FONT_SCALE = 2;
    const float POINT_FROM_FONT_SCALE = 3;
    const float SCREEN_RATIO_SCALE = 0.4;
    const int DEFAULT_SIZE = 35;
    QFont font;
    float defaultFontSize = font.pixelSize() * PIXEL_FROM_FONT_SCALE;
    //increasingly desperate computations:
    if (defaultFontSize <= MIN_SIZE) {
        defaultFontSize = font.pointSize() * POINT_FROM_FONT_SCALE;
        printf("%s - warning, trying QFont.pointSize():%f\n", __func__, defaultFontSize);
    }
    if (defaultFontSize <= MIN_SIZE) {
        QScreen* screen = QGuiApplication::primaryScreen();
        float auxFontSize = SCREEN_RATIO_SCALE * screen->geometry().width();
        defaultFontSize = auxFontSize;
        printf("%s - warning, screen geometry:%f\n", __func__, defaultFontSize);
    }
    if (defaultFontSize <= MIN_SIZE) {
        defaultFontSize = DEFAULT_SIZE;
        printf("%s - warning, will assume dumb size:%f\n", __func__, defaultFontSize);
    }

    return defaultFontSize;
}
