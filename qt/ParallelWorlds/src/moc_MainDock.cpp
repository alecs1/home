/****************************************************************************
** Meta object code from reading C++ file 'MainDock.h'
**
** Created: Sun May 25 15:00:09 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "MainDock.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'MainDock.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MainDock[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x05,
      17,    9,    9,    9, 0x05,
      42,   31,    9,    9, 0x05,
      73,   61,    9,    9, 0x05,

 // slots: signature, parameters, type, tag, flags
     108,    9,    9,    9, 0x0a,
     115,    9,    9,    9, 0x0a,
     123,    9,    9,    9, 0x0a,
     146,    9,    9,    9, 0x0a,
     165,    9,    9,    9, 0x0a,
     184,    9,    9,    9, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MainDock[] = {
    "MainDock\0\0quit()\0toggleFrame()\0"
    "widgetType\0setWidgetType(int)\0newSettings\0"
    "drawSettingsChanged(DrawSettings*)\0"
    "help()\0about()\0toggleShowComponents()\0"
    "toggleWidgetType()\0toggleAutoFillBg()\0"
    "toggleCleanBg()\0"
};

const QMetaObject MainDock::staticMetaObject = {
    { &QDockWidget::staticMetaObject, qt_meta_stringdata_MainDock,
      qt_meta_data_MainDock, 0 }
};

const QMetaObject *MainDock::metaObject() const
{
    return &staticMetaObject;
}

void *MainDock::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MainDock))
	return static_cast<void*>(const_cast< MainDock*>(this));
    if (!strcmp(_clname, "ListerInterface"))
	return static_cast< ListerInterface*>(const_cast< MainDock*>(this));
    return QDockWidget::qt_metacast(_clname);
}

int MainDock::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDockWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: quit(); break;
        case 1: toggleFrame(); break;
        case 2: setWidgetType((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: drawSettingsChanged((*reinterpret_cast< DrawSettings*(*)>(_a[1]))); break;
        case 4: help(); break;
        case 5: about(); break;
        case 6: toggleShowComponents(); break;
        case 7: toggleWidgetType(); break;
        case 8: toggleAutoFillBg(); break;
        case 9: toggleCleanBg(); break;
        }
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void MainDock::quit()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MainDock::toggleFrame()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MainDock::setWidgetType(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void MainDock::drawSettingsChanged(DrawSettings * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}
QT_END_MOC_NAMESPACE
