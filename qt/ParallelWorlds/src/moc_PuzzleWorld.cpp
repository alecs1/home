/****************************************************************************
** Meta object code from reading C++ file 'PuzzleWorld.h'
**
** Created: Sun May 25 15:00:16 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "PuzzleWorld/PuzzleWorld.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'PuzzleWorld.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_PuzzleWorld[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      19,   13,   12,   12, 0x0a,
      44,   38,   12,   12, 0x0a,
      57,   38,   12,   12, 0x0a,
      73,   12,   12,   12, 0x0a,
      86,   12,   12,   12, 0x0a,
     103,   97,   12,   12, 0x0a,
     122,   12,   12,   12, 0x2a,
     131,   12,   12,   12, 0x0a,
     146,   12,   12,   12, 0x0a,
     155,   12,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_PuzzleWorld[] = {
    "PuzzleWorld\0\0fName\0openImage(QString)\0"
    "newNr\0setRows(int)\0setColumns(int)\0"
    "makeRandom()\0solveAll()\0coord\0"
    "gather(Coordinate)\0gather()\0gatherCenter()\0"
    "random()\0randomTogether()\0"
};

const QMetaObject PuzzleWorld::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_PuzzleWorld,
      qt_meta_data_PuzzleWorld, 0 }
};

const QMetaObject *PuzzleWorld::metaObject() const
{
    return &staticMetaObject;
}

void *PuzzleWorld::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_PuzzleWorld))
	return static_cast<void*>(const_cast< PuzzleWorld*>(this));
    if (!strcmp(_clname, "World"))
	return static_cast< World*>(const_cast< PuzzleWorld*>(this));
    return QObject::qt_metacast(_clname);
}

int PuzzleWorld::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: openImage((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 1: setRows((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: setColumns((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: makeRandom(); break;
        case 4: solveAll(); break;
        case 5: gather((*reinterpret_cast< Coordinate(*)>(_a[1]))); break;
        case 6: gather(); break;
        case 7: gatherCenter(); break;
        case 8: random(); break;
        case 9: randomTogether(); break;
        }
        _id -= 10;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
