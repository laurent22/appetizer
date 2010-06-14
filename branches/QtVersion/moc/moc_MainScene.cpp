#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'MainScene.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../gui/MainScene.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'MainScene.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__MainScene[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x0a,
      50,   21,   21,   21, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__MainScene[] = {
    "appetizer::MainScene\0\0resizeSprite_mousePressed()\0"
    "resizeSprite_mouseMoved()\0"
};

const QMetaObject appetizer::MainScene::staticMetaObject = {
    { &GraphicsScene::staticMetaObject, qt_meta_stringdata_appetizer__MainScene,
      qt_meta_data_appetizer__MainScene, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::MainScene::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::MainScene::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::MainScene::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__MainScene))
        return static_cast<void*>(const_cast< MainScene*>(this));
    return GraphicsScene::qt_metacast(_clname);
}

int appetizer::MainScene::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = GraphicsScene::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: resizeSprite_mousePressed(); break;
        case 1: resizeSprite_mouseMoved(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
