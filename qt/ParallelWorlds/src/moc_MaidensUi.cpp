/****************************************************************************
** Meta object code from reading C++ file 'MaidensUi.h'
**
** Created: Sun May 25 15:00:06 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "MCWorld/ui/MaidensUi.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'MaidensUi.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MaidensUi[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x05,
      31,   10,   10,   10, 0x05,
      38,   10,   10,   10, 0x05,
      48,   10,   10,   10, 0x05,
      69,   10,   10,   10, 0x05,
      77,   10,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      86,   10,   10,   10, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MaidensUi[] = {
    "MaidensUi\0\0fileChosen(QString)\0back()\0"
    "forward()\0speedChanged(double)\0start()\0"
    "random()\0openFile()\0"
};

const QMetaObject MaidensUi::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MaidensUi,
      qt_meta_data_MaidensUi, 0 }
};

const QMetaObject *MaidensUi::metaObject() const
{
    return &staticMetaObject;
}

void *MaidensUi::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MaidensUi))
	return static_cast<void*>(const_cast< MaidensUi*>(this));
    if (!strcmp(_clname, "Ui::MaidensForm"))
	return static_cast< Ui::MaidensForm*>(const_cast< MaidensUi*>(this));
    return QWidget::qt_metacast(_clname);
}

int MaidensUi::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fileChosen((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 1: back(); break;
        case 2: forward(); break;
        case 3: speedChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 4: start(); break;
        case 5: random(); break;
        case 6: openFile(); break;
        }
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void MaidensUi::fileChosen(QString _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MaidensUi::back()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MaidensUi::forward()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MaidensUi::speedChanged(double _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void MaidensUi::start()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MaidensUi::random()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}
QT_END_MOC_NAMESPACE
