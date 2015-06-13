#ifndef UTILS_H
#define UTILS_H

#include <QString>


class Utils
{
public:
    struct PointSizeParams {
        QString fontName;
        int targetSize;
        int* nextSmaller;
        int* nextLarger;
        enum Measure {
            width = 0,
            height,
            heightAscent
        };
        Measure measure = width;
        QString text = QString("0");
    };

//    static int getClosestPointSize(QString fontName, int targetSize, int& nextSmaller, int& nextLarger, int direction = 1,
//                                               QString text = QString("O"));
    static int getClosestPointSize(PointSizeParams p);
    //fucking QToolButton looks ridiculous and unclickable on Android with high resolution
    static int estimateQToolButtonSize();
};

#endif // UTILS_H
