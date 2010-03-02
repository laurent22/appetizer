/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __UserSettings_H
#define __UserSettings_H

#include "utilities/XmlUtil.h"



typedef std::map<wxString, wxString> UserSettingsMap;


class UserSettings {

public:

  UserSettings();

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  int GetValidatedIconSize(int iconSize = -1);

  wxString GetString(const wxString& name);
  int GetInt(const wxString& name);
  bool GetBool(const wxString& name);
  wxDateTime GetDateTime(const wxString& name);
  wxColor GetColor(const wxString& name);

  void SetString(const wxString& name, const wxString& value);
  void SetInt(const wxString& name, int value);
  void SetBool(const wxString& name, bool value);
  void SetDateTime(const wxString& name, const wxDateTime& dateTime);
  void SetColor(const wxString& name, wxColor& color);

private:

  void AppendSettingToXml(TiXmlElement* element, const char* name, const char* value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, wxString value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, bool value);
  void AssignSettingValue(int& setting, wxString value, int defaultValue);
  void AssignSettingValue(int& setting, wxString value);

  bool ParseBoolean(const wxString& toParse);

  UserSettingsMap values_;

};



#endif // __UserSettings_H