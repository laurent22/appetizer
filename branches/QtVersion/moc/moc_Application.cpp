#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'Application.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../Application.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Application.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__Application[] = {

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
      24,   23,   23,   23, 0x08,
      47,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__Application[] = {
    "appetizer::Application\0\0closingTimer_timeout()\0"
    "minimizingTimer_timeout()\0"
};

const QMetaObject appetizer::Application::staticMetaObject = {
    { &QApplication::staticMetaObject, qt_meta_stringdata_appetizer__Application,
      qt_meta_data_appetizer__Application, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::Application::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::Application::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::Application::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__Application))
        return static_cast<void*>(const_cast< Application*>(this));
    return QApplication::qt_metacast(_clname);
}

int appetizer::Application::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QApplication::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: closingTimer_timeout(); break;
        case 1: minimizingTimer_timeout(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
