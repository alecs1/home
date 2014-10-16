/****************************************************************************
** Meta object code from reading C++ file 'UniverseShowerGL.h'
**
** Created: Sun May 25 15:00:29 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "UniverseShowerGL.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'UniverseShowerGL.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_UniverseShowerGL[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      30,   18,   17,   17, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_UniverseShowerGL[] = {
    "UniverseShowerGL\0\0newSettings\0"
    "settingsChanged(DrawSettings*)\0"
};

const QMetaObject UniverseShowerGL::staticMetaObject = {
    { &QGLWidget::staticMetaObject, qt_meta_stringdata_UniverseShowerGL,
      qt_meta_data_UniverseShowerGL, 0 }
};

const QMetaObject *UniverseShowerGL::metaObject() const
{
    return &staticMetaObject;
}

void *UniverseShowerGL::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_UniverseShowerGL))
	return static_cast<void*>(const_cast< UniverseShowerGL*>(this));
    return QGLWidget::qt_metacast(_clname);
}

int UniverseShowerGL::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGLWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: settingsChanged((*reinterpret_cast< DrawSettings*(*)>(_a[1]))); break;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
