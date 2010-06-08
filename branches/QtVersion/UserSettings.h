/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_UserSettings_H
#define Appetizer_UserSettings_H


namespace appetizer {


typedef std::map<QString, QString> UserSettingsMap;

class UserSettings {

public:

  static UserSettings* instance();

  UserSettings();

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

private:

  void AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, QString value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, bool value);
  void AssignSettingValue(int& setting, QString value, int defaultValue);
  void AssignSettingValue(int& setting, QString value);

  bool ParseBoolean(const QString& toParse);

  UserSettingsMap values_;

  static UserSettings* instance_;

};

}

#endif // __UserSettings_H