#include "HelpDialog.h"

#include "Global.h"

#include <QGridLayout>
#include <QTextEdit>
#include <QPushButton>




//find the main window so we can create the dialog as large as the main window is
#include <QMainWindow>
#include <QApplication>
static QMainWindow* getMainWindow()
{
    QWidgetList widgets = qApp->topLevelWidgets();
    for (QWidgetList::iterator i = widgets.begin(); i != widgets.end(); ++i)
        if ((*i)->objectName() == "MainWindow")
            return (QMainWindow*) (*i);
    return NULL;
}



HelpDialog::HelpDialog(QWidget* parent) : QDialog(parent)
{
    printf("%s\n", __func__);
    if (layout() != NULL)
        delete layout();

    QGridLayout* gridLayout= new QGridLayout();
    setLayout(gridLayout);

    okButton = new QPushButton("Close", this);
    gridLayout->addWidget(okButton, 2, 0, 1, 2);


    QString contentsText =
"Starting a game:\n\
Either press start, or tap the table to place a stone and confirm the move.\n\
\n\n\
Ending a game:\n\
The game ends after two consecutive passes (you and he opponent have passed one after the other).\n\
You can also choose \"Resign\", in which case you lose. If the game is sufficiently advanced, you'll see an \"estimate\" score for the current table situation.\n\
\n\n\
Player types:\
Computer: Play against the program, in which case you can choose its strength (with the strongest around 13k).\n\
Humans: Two humans playing on the same screem.\n\
Network: Not available yet.\n\
\n\n\
Game rules:\n\
https://www.gokgs.com/tutorial/index.jsp\n\
\n\n";


    contents = new QTextEdit();
    contents->setPlainText(contentsText);
    //contents->setAlignment(Qt::AlignCenter);
    gridLayout->addWidget(contents, 0, 0, 1, 2);


    connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));

    QMainWindow* mainWindow = getMainWindow();
    if (mainWindow != NULL) {
        resize(mainWindow->size());
    }
    else {
        printf("%s - error, could not find MainWindow\n", __func__);
    }

}

