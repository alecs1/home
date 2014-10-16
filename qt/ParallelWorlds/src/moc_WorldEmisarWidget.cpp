/****************************************************************************
** Meta object code from reading C++ file 'WorldEmisarWidget.h'
**
** Created: Sun May 25 15:00:12 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "WorldEmisarWidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'WorldEmisarWidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_WorldEmisarWidget[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      19,   18,   18,   18, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_WorldEmisarWidget[] = {
    "WorldEmisarWidget\0\0makeInstance()\0"
};

const QMetaObject WorldEmisarWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_WorldEmisarWidget,
      qt_meta_data_WorldEmisarWidget, 0 }
};

const QMetaObject *WorldEmisarWidget::metaObject() const
{
    return &staticMetaObject;
}

void *WorldEmisarWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_WorldEmisarWidget))
	return static_cast<void*>(const_cast< WorldEmisarWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int WorldEmisarWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: makeInstance(); break;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
