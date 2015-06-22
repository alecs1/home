#ifndef MINISETTINGSWIDGET_H
#define MINISETTINGSWIDGET_H

#include <QWidget>

class QToolButton;
class RoundInfo;
class QVBoxLayout;

class MiniGameSettings : public QWidget {
Q_OBJECT
public:
    explicit MiniGameSettings(QWidget* parent);
    void addRoundInfo(RoundInfo* aRoundInfo);
public slots:
    void changeProgramSettings();
private:
    RoundInfo* roundInfo = NULL;
    QToolButton* passButton = NULL;
    QToolButton* undoButton = NULL;
    QToolButton* fullInterfaceButton = NULL;
    QVBoxLayout* layoutP;
};

#endif // MINISETTINGSWIDGET_H
