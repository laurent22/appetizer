/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_UserSettings_H
#define Appetizer_UserSettings_H


namespace appetizer {


class UserSetting {
  
public:

  UserSetting(const QString& name);
  inline QString name() const { return name_; }
  inline QVariant value() const { return value_; }
  void setValue(const QVariant& variant);

private:

  QString name_;
  QVariant value_;

};


typedef std::map<QString, UserSetting*> UserSettingsMap;

class UserSettings {

public:

  static UserSettings* instance();
  static void destroyInstance();

  UserSettings();
  ~UserSettings();

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  int GetValidatedIconSize(int iconSize = -1);

  UserSetting* getSetting(const QString& name) const;
  void setSetting(const QString& name, const QVariant& variant);
  
  QVariant::Type getSettingType(const QString& name) const;
  QString typeToString(QVariant::Type type) const;

  int getInt(const QString& name) const;
  QString getString(const QString& name) const;
  bool getBool(const QString& name) const;
  QDateTime getDateTime(const QString& name) const;
  QColor getColor(const QString& name) const;

  void setInt(const QString& name, int value);
  void setString(const QString& name, const QString& value);
  void setBool(const QString& name, bool value);
  void setDateTime(const QString& name, const QDateTime& dateTime);
  void setColor(const QString& name, const QColor& color);

private:

  void AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value, int type);

  mutable UserSettingsMap settings_;
  static UserSettings* instance_;

};

}

#endif // __UserSettings_H