/********************************************************************************
** Form generated from reading UI file 'puzzle.ui'
**
** Created by: Qt User Interface Compiler version 5.3.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PUZZLE_H
#define UI_PUZZLE_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_PuzzleForm
{
public:
    QWidget *layoutWidget;
    QVBoxLayout *vboxLayout;
    QVBoxLayout *vboxLayout1;
    QPushButton *selectPuzzleButton;
    QPushButton *randPozButton;
    QPushButton *randMoveButton;
    QToolButton *solveButton;
    QToolButton *parallelSolveButton;
    QToolButton *solutionSolveButton;
    QWidget *layoutWidget1;
    QVBoxLayout *vboxLayout2;
    QLabel *rowLabel;
    QSpinBox *rowsSpinBox;
    QWidget *layoutWidget2;
    QVBoxLayout *vboxLayout3;
    QLabel *colLabel;
    QSpinBox *colsSpinBox;
    QPushButton *gatherButton;
    QPushButton *randGatherButton;
    QPushButton *openButton;

    void setupUi(QWidget *PuzzleForm)
    {
        if (PuzzleForm->objectName().isEmpty())
            PuzzleForm->setObjectName(QStringLiteral("PuzzleForm"));
        PuzzleForm->resize(203, 321);
        const QIcon icon = QIcon(QString::fromUtf8(":/images/icons/parallel worlds.png"));
        PuzzleForm->setWindowIcon(icon);
        layoutWidget = new QWidget(PuzzleForm);
        layoutWidget->setObjectName(QStringLiteral("layoutWidget"));
        layoutWidget->setGeometry(QRect(10, 50, 127, 193));
        vboxLayout = new QVBoxLayout(layoutWidget);
#ifndef Q_OS_MAC
        vboxLayout->setSpacing(6);
#endif
        vboxLayout->setContentsMargins(0, 0, 0, 0);
        vboxLayout->setObjectName(QStringLiteral("vboxLayout"));
        vboxLayout->setContentsMargins(0, 0, 0, 0);
        vboxLayout1 = new QVBoxLayout();
#ifndef Q_OS_MAC
        vboxLayout1->setSpacing(6);
#endif
#ifndef Q_OS_MAC
        vboxLayout1->setContentsMargins(0, 0, 0, 0);
#endif
        vboxLayout1->setObjectName(QStringLiteral("vboxLayout1"));
        selectPuzzleButton = new QPushButton(layoutWidget);
        selectPuzzleButton->setObjectName(QStringLiteral("selectPuzzleButton"));

        vboxLayout1->addWidget(selectPuzzleButton);

        randPozButton = new QPushButton(layoutWidget);
        randPozButton->setObjectName(QStringLiteral("randPozButton"));

        vboxLayout1->addWidget(randPozButton);

        randMoveButton = new QPushButton(layoutWidget);
        randMoveButton->setObjectName(QStringLiteral("randMoveButton"));

        vboxLayout1->addWidget(randMoveButton);


        vboxLayout->addLayout(vboxLayout1);

        solveButton = new QToolButton(layoutWidget);
        solveButton->setObjectName(QStringLiteral("solveButton"));

        vboxLayout->addWidget(solveButton);

        parallelSolveButton = new QToolButton(layoutWidget);
        parallelSolveButton->setObjectName(QStringLiteral("parallelSolveButton"));

        vboxLayout->addWidget(parallelSolveButton);

        solutionSolveButton = new QToolButton(layoutWidget);
        solutionSolveButton->setObjectName(QStringLiteral("solutionSolveButton"));

        vboxLayout->addWidget(solutionSolveButton);

        layoutWidget1 = new QWidget(PuzzleForm);
        layoutWidget1->setObjectName(QStringLiteral("layoutWidget1"));
        layoutWidget1->setGeometry(QRect(150, 70, 48, 50));
        vboxLayout2 = new QVBoxLayout(layoutWidget1);
#ifndef Q_OS_MAC
        vboxLayout2->setSpacing(6);
#endif
        vboxLayout2->setContentsMargins(0, 0, 0, 0);
        vboxLayout2->setObjectName(QStringLiteral("vboxLayout2"));
        vboxLayout2->setContentsMargins(0, 0, 0, 0);
        rowLabel = new QLabel(layoutWidget1);
        rowLabel->setObjectName(QStringLiteral("rowLabel"));

        vboxLayout2->addWidget(rowLabel);

        rowsSpinBox = new QSpinBox(layoutWidget1);
        rowsSpinBox->setObjectName(QStringLiteral("rowsSpinBox"));
        rowsSpinBox->setMaximum(500);
        rowsSpinBox->setMinimum(1);
        rowsSpinBox->setValue(10);

        vboxLayout2->addWidget(rowsSpinBox);

        layoutWidget2 = new QWidget(PuzzleForm);
        layoutWidget2->setObjectName(QStringLiteral("layoutWidget2"));
        layoutWidget2->setGeometry(QRect(150, 140, 48, 50));
        vboxLayout3 = new QVBoxLayout(layoutWidget2);
#ifndef Q_OS_MAC
        vboxLayout3->setSpacing(6);
#endif
        vboxLayout3->setContentsMargins(0, 0, 0, 0);
        vboxLayout3->setObjectName(QStringLiteral("vboxLayout3"));
        vboxLayout3->setContentsMargins(0, 0, 0, 0);
        colLabel = new QLabel(layoutWidget2);
        colLabel->setObjectName(QStringLiteral("colLabel"));

        vboxLayout3->addWidget(colLabel);

        colsSpinBox = new QSpinBox(layoutWidget2);
        colsSpinBox->setObjectName(QStringLiteral("colsSpinBox"));
        colsSpinBox->setMaximum(500);
        colsSpinBox->setMinimum(1);
        colsSpinBox->setValue(10);

        vboxLayout3->addWidget(colsSpinBox);

        gatherButton = new QPushButton(PuzzleForm);
        gatherButton->setObjectName(QStringLiteral("gatherButton"));
        gatherButton->setGeometry(QRect(10, 250, 85, 27));
        randGatherButton = new QPushButton(PuzzleForm);
        randGatherButton->setObjectName(QStringLiteral("randGatherButton"));
        randGatherButton->setGeometry(QRect(10, 290, 145, 27));
        openButton = new QPushButton(PuzzleForm);
        openButton->setObjectName(QStringLiteral("openButton"));
        openButton->setGeometry(QRect(10, 10, 100, 27));
        const QIcon icon1 = QIcon(QString::fromUtf8(":/images/icons/folder_crystal.png"));
        openButton->setIcon(icon1);
#ifndef QT_NO_SHORTCUT
        rowLabel->setBuddy(rowsSpinBox);
        colLabel->setBuddy(colsSpinBox);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(openButton, randPozButton);
        QWidget::setTabOrder(randPozButton, randMoveButton);
        QWidget::setTabOrder(randMoveButton, solveButton);
        QWidget::setTabOrder(solveButton, parallelSolveButton);
        QWidget::setTabOrder(parallelSolveButton, solutionSolveButton);
        QWidget::setTabOrder(solutionSolveButton, rowsSpinBox);
        QWidget::setTabOrder(rowsSpinBox, colsSpinBox);

        retranslateUi(PuzzleForm);

        QMetaObject::connectSlotsByName(PuzzleForm);
    } // setupUi

    void retranslateUi(QWidget *PuzzleForm)
    {
        PuzzleForm->setWindowTitle(QApplication::translate("PuzzleForm", "Puzzle World", 0));
        selectPuzzleButton->setText(QApplication::translate("PuzzleForm", "Select puzzle", 0));
        randPozButton->setText(QApplication::translate("PuzzleForm", "Amestec\304\203", 0));
        randMoveButton->setText(QApplication::translate("PuzzleForm", "Move randomly", 0));
        solveButton->setText(QApplication::translate("PuzzleForm", "Solve one at a time", 0));
#ifndef QT_NO_TOOLTIP
        parallelSolveButton->setToolTip(QApplication::translate("PuzzleForm", "All pieces go to their place in the same time", 0));
#endif // QT_NO_TOOLTIP
        parallelSolveButton->setText(QApplication::translate("PuzzleForm", "Solve", 0));
#ifndef QT_NO_TOOLTIP
        solutionSolveButton->setToolTip(QApplication::translate("PuzzleForm", "Load a solution and display it", 0));
#endif // QT_NO_TOOLTIP
        solutionSolveButton->setText(QApplication::translate("PuzzleForm", "Solve from solution", 0));
        rowLabel->setText(QApplication::translate("PuzzleForm", "Rows", 0));
        colLabel->setText(QApplication::translate("PuzzleForm", "Columns", 0));
        gatherButton->setText(QApplication::translate("PuzzleForm", "Gather pieces", 0));
        randGatherButton->setText(QApplication::translate("PuzzleForm", "Move randomly together", 0));
        openButton->setText(QApplication::translate("PuzzleForm", "Select image", 0));
    } // retranslateUi

};

namespace Ui {
    class PuzzleForm: public Ui_PuzzleForm {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PUZZLE_H
