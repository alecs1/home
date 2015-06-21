#ifndef MINISETTINGSWIDGET_H
#define MINISETTINGSWIDGET_H

#include <QWidget>

class QToolButton;
class QVBoxLayout;

class MiniGameSettings : public QWidget {
Q_OBJECT
public:
    explicit MiniGameSettings(QWidget* parent);
private:
    QVBoxLayout* layout = NULL;
    QToolButton* passButton = NULL;
    QToolButton* undoButton = NULL;
    QToolButton* fullInterface = NULL;
};

#endif // MINISETTINGSWIDGET_H
