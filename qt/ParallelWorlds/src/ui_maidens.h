/********************************************************************************
** Form generated from reading UI file 'maidens.ui'
**
** Created by: Qt User Interface Compiler version 5.3.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAIDENS_H
#define UI_MAIDENS_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MaidensForm
{
public:
    QGridLayout *gridLayout;
    QVBoxLayout *vboxLayout;
    QPushButton *openButton;
    QHBoxLayout *hboxLayout;
    QDoubleSpinBox *speedSpinBox;
    QPushButton *startButton;
    QHBoxLayout *hboxLayout1;
    QToolButton *backButton;
    QToolButton *forwardButton;
    QPushButton *randomButton;

    void setupUi(QWidget *MaidensForm)
    {
        if (MaidensForm->objectName().isEmpty())
            MaidensForm->setObjectName(QStringLiteral("MaidensForm"));
        MaidensForm->resize(164, 149);
        QSizePolicy sizePolicy(static_cast<QSizePolicy::Policy>(3), static_cast<QSizePolicy::Policy>(3));
        sizePolicy.setHorizontalStretch(1);
        sizePolicy.setVerticalStretch(1);
        sizePolicy.setHeightForWidth(MaidensForm->sizePolicy().hasHeightForWidth());
        MaidensForm->setSizePolicy(sizePolicy);
        const QIcon icon = QIcon(QString::fromUtf8(":/images/icons/parallel worlds.png"));
        MaidensForm->setWindowIcon(icon);
        gridLayout = new QGridLayout(MaidensForm);
#ifndef Q_OS_MAC
        gridLayout->setSpacing(6);
#endif
#ifndef Q_OS_MAC
        gridLayout->setContentsMargins(9, 9, 9, 9);
#endif
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        vboxLayout = new QVBoxLayout();
#ifndef Q_OS_MAC
        vboxLayout->setSpacing(6);
#endif
#ifndef Q_OS_MAC
        vboxLayout->setContentsMargins(0, 0, 0, 0);
#endif
        vboxLayout->setObjectName(QStringLiteral("vboxLayout"));
        openButton = new QPushButton(MaidensForm);
        openButton->setObjectName(QStringLiteral("openButton"));

        vboxLayout->addWidget(openButton);

        hboxLayout = new QHBoxLayout();
#ifndef Q_OS_MAC
        hboxLayout->setSpacing(6);
#endif
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        hboxLayout->setObjectName(QStringLiteral("hboxLayout"));
        speedSpinBox = new QDoubleSpinBox(MaidensForm);
        speedSpinBox->setObjectName(QStringLiteral("speedSpinBox"));
        speedSpinBox->setMaximum(400);
        speedSpinBox->setMinimum(1);
        speedSpinBox->setValue(60);

        hboxLayout->addWidget(speedSpinBox);

        startButton = new QPushButton(MaidensForm);
        startButton->setObjectName(QStringLiteral("startButton"));

        hboxLayout->addWidget(startButton);


        vboxLayout->addLayout(hboxLayout);

        hboxLayout1 = new QHBoxLayout();
#ifndef Q_OS_MAC
        hboxLayout1->setSpacing(6);
#endif
        hboxLayout1->setContentsMargins(0, 0, 0, 0);
        hboxLayout1->setObjectName(QStringLiteral("hboxLayout1"));
        backButton = new QToolButton(MaidensForm);
        backButton->setObjectName(QStringLiteral("backButton"));

        hboxLayout1->addWidget(backButton);

        forwardButton = new QToolButton(MaidensForm);
        forwardButton->setObjectName(QStringLiteral("forwardButton"));

        hboxLayout1->addWidget(forwardButton);


        vboxLayout->addLayout(hboxLayout1);

        randomButton = new QPushButton(MaidensForm);
        randomButton->setObjectName(QStringLiteral("randomButton"));

        vboxLayout->addWidget(randomButton);


        gridLayout->addLayout(vboxLayout, 0, 0, 1, 1);

        QWidget::setTabOrder(openButton, speedSpinBox);
        QWidget::setTabOrder(speedSpinBox, startButton);
        QWidget::setTabOrder(startButton, backButton);
        QWidget::setTabOrder(backButton, forwardButton);

        retranslateUi(MaidensForm);

        QMetaObject::connectSlotsByName(MaidensForm);
    } // setupUi

    void retranslateUi(QWidget *MaidensForm)
    {
        MaidensForm->setWindowTitle(QApplication::translate("MaidensForm", "Maidens and Italians", 0));
        openButton->setText(QApplication::translate("MaidensForm", "Open", 0));
        startButton->setText(QApplication::translate("MaidensForm", "Start", 0));
#ifndef QT_NO_TOOLTIP
        backButton->setToolTip(QApplication::translate("MaidensForm", "Back", 0));
#endif // QT_NO_TOOLTIP
        backButton->setText(QApplication::translate("MaidensForm", "Back", 0));
#ifndef QT_NO_TOOLTIP
        forwardButton->setToolTip(QApplication::translate("MaidensForm", "Forward", 0));
#endif // QT_NO_TOOLTIP
        forwardButton->setText(QApplication::translate("MaidensForm", "Forward", 0));
        randomButton->setText(QApplication::translate("MaidensForm", "Random", 0));
    } // retranslateUi

};

namespace Ui {
    class MaidensForm: public Ui_MaidensForm {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAIDENS_H
