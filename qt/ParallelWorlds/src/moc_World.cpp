/****************************************************************************
** Meta object code from reading C++ file 'World.h'
**
** Created: Mon Jun 11 11:35:15 2007
**      by: The Qt Meta Object Compiler version 59 (Qt 4.2.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "World.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'World.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.2.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

static const uint qt_meta_data_WorldEmisar[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      25,   13,   12,   12, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_WorldEmisar[] = {
    "WorldEmisar\0\0newInstance\0instanceCreated(World*)\0"
};

const QMetaObject WorldEmisar::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_WorldEmisar,
      qt_meta_data_WorldEmisar, 0 }
};

const QMetaObject *WorldEmisar::metaObject() const
{
    return &staticMetaObject;
}

void *WorldEmisar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_WorldEmisar))
	return static_cast<void*>(const_cast<WorldEmisar*>(this));
    return QObject::qt_metacast(_clname);
}

int WorldEmisar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: instanceCreated((*reinterpret_cast< World*(*)>(_a[1]))); break;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void WorldEmisar::instanceCreated(World * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
