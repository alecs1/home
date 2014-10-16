/****************************************************************************
** Meta object code from reading C++ file 'UniverseWidgetBase.h'
**
** Created: Sun May 25 15:00:26 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "UniverseWidgetBase.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'UniverseWidgetBase.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_UniverseWidgetBase[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      20,   19,   19,   19, 0x09,
      27,   19,   19,   19, 0x09,
      57,   45,   19,   19, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_UniverseWidgetBase[] = {
    "UniverseWidgetBase\0\0step()\0toggleFrameMode()\0"
    "newSettings\0drawSettingsChanged(DrawSettings*)\0"
};

const QMetaObject UniverseWidgetBase::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_UniverseWidgetBase,
      qt_meta_data_UniverseWidgetBase, 0 }
};

const QMetaObject *UniverseWidgetBase::metaObject() const
{
    return &staticMetaObject;
}

void *UniverseWidgetBase::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_UniverseWidgetBase))
	return static_cast<void*>(const_cast< UniverseWidgetBase*>(this));
    return QWidget::qt_metacast(_clname);
}

int UniverseWidgetBase::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: step(); break;
        case 1: toggleFrameMode(); break;
        case 2: drawSettingsChanged((*reinterpret_cast< DrawSettings*(*)>(_a[1]))); break;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
