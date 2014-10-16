/****************************************************************************
** Meta object code from reading C++ file 'MCWorld.h'
**
** Created: Sun May 25 15:00:03 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "MCWorld/MCWorld.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'MCWorld.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MCWorld[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      14,    9,    8,    8, 0x08,
      32,    8,    8,    8, 0x08,
      46,    8,    8,    8, 0x08,
      53,    8,    8,    8, 0x08,
      72,   63,    8,    8, 0x08,
      92,    8,    8,    8, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MCWorld[] = {
    "MCWorld\0\0file\0openFile(QString)\0"
    "toggleStart()\0back()\0forward()\0newSpeed\0"
    "changeSpeed(double)\0random()\0"
};

const QMetaObject MCWorld::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MCWorld,
      qt_meta_data_MCWorld, 0 }
};

const QMetaObject *MCWorld::metaObject() const
{
    return &staticMetaObject;
}

void *MCWorld::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MCWorld))
	return static_cast<void*>(const_cast< MCWorld*>(this));
    if (!strcmp(_clname, "World"))
	return static_cast< World*>(const_cast< MCWorld*>(this));
    return QObject::qt_metacast(_clname);
}

int MCWorld::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: openFile((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 1: toggleStart(); break;
        case 2: back(); break;
        case 3: forward(); break;
        case 4: changeSpeed((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 5: random(); break;
        }
        _id -= 6;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
