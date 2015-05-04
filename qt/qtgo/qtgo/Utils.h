#ifndef UTILS_H
#define UTILS_H

#include <QString>


class Utils
{
public:
    static int getClosestPointSize(QString fontName, int targetSize, int& nextSmaller, int& nextLarger, int direction = 1,
                                               QString text = QString("O"));
    //fucking QToolButton looks ridiculous and unclickable on Android with high resolution
    static int estimateQToolButtonSize();
};

#endif // UTILS_H
