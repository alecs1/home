#include <QWidget>


class Main : public QWidget {
    Q_OBJECT
public:
    Main(QWidget* parent = 0);

private:
    QPushButton* button;

public slots:
    int buttonClicked();
    

}
