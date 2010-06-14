#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'ScrollBar.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../gui/ScrollBar.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ScrollBar.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__ScrollBar[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
      37,   21,   21,   21, 0x09,
      57,   21,   21,   21, 0x09,
      75,   21,   21,   21, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__ScrollBar[] = {
    "appetizer::ScrollBar\0\0valueChanged()\0"
    "knob_mousePressed()\0knob_mouseMoved()\0"
    "knob_mouseReleased()\0"
};

const QMetaObject appetizer::ScrollBar::staticMetaObject = {
    { &GraphicsItem::staticMetaObject, qt_meta_stringdata_appetizer__ScrollBar,
      qt_meta_data_appetizer__ScrollBar, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::ScrollBar::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::ScrollBar::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::ScrollBar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__ScrollBar))
        return static_cast<void*>(const_cast< ScrollBar*>(this));
    return GraphicsItem::qt_metacast(_clname);
}

int appetizer::ScrollBar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = GraphicsItem::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: valueChanged(); break;
        case 1: knob_mousePressed(); break;
        case 2: knob_mouseMoved(); break;
        case 3: knob_mouseReleased(); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void appetizer::ScrollBar::valueChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
