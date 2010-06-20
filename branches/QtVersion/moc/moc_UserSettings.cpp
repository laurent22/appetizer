#include <stable.h>
/****************************************************************************
** Meta object code from reading C++ file 'UserSettings.h'
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../UserSettings.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'UserSettings.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appetizer__UserSetting[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      24,   23,   23,   23, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__UserSetting[] = {
    "appetizer::UserSetting\0\0valueChanged()\0"
};

const QMetaObject appetizer::UserSetting::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appetizer__UserSetting,
      qt_meta_data_appetizer__UserSetting, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::UserSetting::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::UserSetting::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::UserSetting::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__UserSetting))
        return static_cast<void*>(const_cast< UserSetting*>(this));
    return QObject::qt_metacast(_clname);
}

int appetizer::UserSetting::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: valueChanged(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void appetizer::UserSetting::valueChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
static const uint qt_meta_data_appetizer__UserSettings[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      33,   25,   24,   24, 0x05,

 // slots: signature, parameters, type, tag, flags
      62,   24,   24,   24, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appetizer__UserSettings[] = {
    "appetizer::UserSettings\0\0setting\0"
    "settingChanged(UserSetting*)\0"
    "setting_valueChanged()\0"
};

const QMetaObject appetizer::UserSettings::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appetizer__UserSettings,
      qt_meta_data_appetizer__UserSettings, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appetizer::UserSettings::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appetizer::UserSettings::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appetizer::UserSettings::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appetizer__UserSettings))
        return static_cast<void*>(const_cast< UserSettings*>(this));
    return QObject::qt_metacast(_clname);
}

int appetizer::UserSettings::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: settingChanged((*reinterpret_cast< UserSetting*(*)>(_a[1]))); break;
        case 1: setting_valueChanged(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void appetizer::UserSettings::settingChanged(UserSetting * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
