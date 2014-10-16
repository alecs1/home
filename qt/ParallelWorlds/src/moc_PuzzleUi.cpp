/****************************************************************************
** Meta object code from reading C++ file 'PuzzleUi.h'
**
** Created: Sun May 25 15:00:20 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0-rc1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "PuzzleWorld/ui/PuzzleUi.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'PuzzleUi.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0-rc1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_PuzzleUi[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      16,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x05,
      30,    9,    9,    9, 0x05,
      52,    9,    9,    9, 0x05,
      64,    9,    9,    9, 0x05,
      76,    9,    9,    9, 0x05,
      89,    9,    9,    9, 0x05,
     104,    9,    9,    9, 0x05,
     112,    9,    9,    9, 0x05,
     128,    9,    9,    9, 0x05,
     155,    9,    9,    9, 0x05,
     172,    9,    9,    9, 0x05,
     189,    9,    9,    9, 0x05,
     198,    9,    9,    9, 0x05,

 // slots: signature, parameters, type, tag, flags
     221,    9,    9,    9, 0x08,
     233,    9,    9,    9, 0x08,
     248,    9,    9,    9, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_PuzzleUi[] = {
    "PuzzleUi\0\0fileChosen(QString)\0"
    "puzzleChosen(QString)\0newRow(int)\0"
    "newCol(int)\0makePuzzle()\0moveRandomly()\0"
    "solve()\0solveParallel()\0"
    "solveFromSolution(QString)\0changeNrRow(int)\0"
    "changeNrCol(int)\0gather()\0"
    "moveRandomlyTogether()\0openImage()\0"
    "openSolution()\0openPuzzle()\0"
};

const QMetaObject PuzzleUi::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_PuzzleUi,
      qt_meta_data_PuzzleUi, 0 }
};

const QMetaObject *PuzzleUi::metaObject() const
{
    return &staticMetaObject;
}

void *PuzzleUi::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_PuzzleUi))
	return static_cast<void*>(const_cast< PuzzleUi*>(this));
    if (!strcmp(_clname, "Ui::PuzzleForm"))
	return static_cast< Ui::PuzzleForm*>(const_cast< PuzzleUi*>(this));
    return QWidget::qt_metacast(_clname);
}

int PuzzleUi::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fileChosen((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 1: puzzleChosen((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 2: newRow((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: newCol((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: makePuzzle(); break;
        case 5: moveRandomly(); break;
        case 6: solve(); break;
        case 7: solveParallel(); break;
        case 8: solveFromSolution((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 9: changeNrRow((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 10: changeNrCol((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 11: gather(); break;
        case 12: moveRandomlyTogether(); break;
        case 13: openImage(); break;
        case 14: openSolution(); break;
        case 15: openPuzzle(); break;
        }
        _id -= 16;
    }
    return _id;
}

// SIGNAL 0
void PuzzleUi::fileChosen(QString _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void PuzzleUi::puzzleChosen(QString _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void PuzzleUi::newRow(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void PuzzleUi::newCol(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void PuzzleUi::makePuzzle()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void PuzzleUi::moveRandomly()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void PuzzleUi::solve()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}

// SIGNAL 7
void PuzzleUi::solveParallel()
{
    QMetaObject::activate(this, &staticMetaObject, 7, 0);
}

// SIGNAL 8
void PuzzleUi::solveFromSolution(QString _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void PuzzleUi::changeNrRow(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void PuzzleUi::changeNrCol(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}

// SIGNAL 11
void PuzzleUi::gather()
{
    QMetaObject::activate(this, &staticMetaObject, 11, 0);
}

// SIGNAL 12
void PuzzleUi::moveRandomlyTogether()
{
    QMetaObject::activate(this, &staticMetaObject, 12, 0);
}
QT_END_MOC_NAMESPACE
