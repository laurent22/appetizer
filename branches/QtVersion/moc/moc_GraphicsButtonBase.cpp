#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'GraphicsButtonBase.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../guilib/GraphicsButtonBase.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'GraphicsButtonBase.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__GraphicsButtonBase[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

static const char qt_meta_stringdata_appetizer__GraphicsButtonBase[] = {
    "appetizer::GraphicsButtonBase\0"
};

const QMetaObject appetizer::GraphicsButtonBase::staticMetaObject = {
    { &GraphicsItem::staticMetaObject, qt_meta_stringdata_appetizer__GraphicsButtonBase,
      qt_meta_data_appetizer__GraphicsButtonBase, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::GraphicsButtonBase::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::GraphicsButtonBase::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::GraphicsButtonBase::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__GraphicsButtonBase))
        return static_cast<void*>(const_cast< GraphicsButtonBase*>(this));
    return GraphicsItem::qt_metacast(_clname);
}

int appetizer::GraphicsButtonBase::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = GraphicsItem::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
