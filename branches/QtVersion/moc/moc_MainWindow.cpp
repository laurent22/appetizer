#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'MainWindow.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../gui/MainWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'MainWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__MainWindow[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      28,   23,   22,   22, 0x0a,
      59,   22,   22,   22, 0x0a,
      91,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__MainWindow[] = {
    "appetizer::MainWindow\0\0rect\0"
    "scene_sceneRectChanged(QRectF)\0"
    "backgroundSprite_mousePressed()\0"
    "backgroundSprite_mouseMoved()\0"
};

const QMetaObject appetizer::MainWindow::staticMetaObject = {
    { &GraphicsWindow::staticMetaObject, qt_meta_stringdata_appetizer__MainWindow,
      qt_meta_data_appetizer__MainWindow, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::MainWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::MainWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::MainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__MainWindow))
        return static_cast<void*>(const_cast< MainWindow*>(this));
    return GraphicsWindow::qt_metacast(_clname);
}

int appetizer::MainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = GraphicsWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: scene_sceneRectChanged((*reinterpret_cast< const QRectF(*)>(_a[1]))); break;
        case 1: backgroundSprite_mousePressed(); break;
        case 2: backgroundSprite_mouseMoved(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
