/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_UserSettings_H
#define Appetizer_UserSettings_H


namespace appetizer {


struct UserSetting {
  int type;
  QString value;
};


typedef std::map<QString, UserSetting*> UserSettingsMap;

class UserSettings {

public:

  enum {
    Type_Int,
    Type_String,
    Type_Bool,
    Type_Date,
    Type_Color
  };

  static UserSettings* instance();

  UserSettings();
  ~UserSettings();

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  int GetValidatedIconSize(int iconSize = -1);

  QString GetString(const QString& name);
  int GetInt(const QString& name);
  bool GetBool(const QString& name);
  QDate GetDateTime(const QString& name);
  QColor GetColor(const QString& name);

  void SetString(const QString& name, const QString& value);
  void SetInt(const QString& name, int value);
  void SetBool(const QString& name, bool value);
  void SetDateTime(const QString& name, const QDate& dateTime);
  void SetValue(const QString& name, const QString& value, int type);

private:

  void AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value, int type);
  //void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  //void AppendSettingToXml(TiXmlElement* element, const char* name, QString value);
  //void AppendSettingToXml(TiXmlElement* element, const char* name, bool value);

  UserSettingsMap values_;

  static UserSettings* instance_;

};

}

#endif // __UserSettings_H