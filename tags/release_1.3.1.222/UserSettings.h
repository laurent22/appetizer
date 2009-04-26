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

  //int IconSize;
  //wxString Locale;
  //wxString PortableAppsPath;
  //wxString DocumentsPath;
  //wxString MusicPath;
  //wxString PicturesPath;
  //wxString VideosPath;
  //wxString Skin;
  //bool MinimizeOnClose;
  //bool Rotated;
  //wxDateTime NextUpdateCheckTime;
  //bool AlwaysOnTop;
  //bool AutoHideApplication;
  //bool UniqueApplicationInstance;
  //bool ShowDeleteIconMessage;
  //bool ShowEjectDriveMessage;
  //bool ShowMinimizeMessage;
  //bool RunMultiLaunchOnStartUp;
  //bool CloseAppsOnEject;
  //bool HotKeyControl;
  //bool HotKeyAlt;
  //bool HotKeyShift;
  //int HotKeyKey;

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  int GetValidatedIconSize();

  wxString GetString(const wxString& name);
  int GetInt(const wxString& name);
  bool GetBool(const wxString& name);
  wxDateTime GetDateTime(const wxString& name);

  void SetString(const wxString& name, const wxString& value);
  void SetInt(const wxString& name, int value);
  void SetBool(const wxString& name, bool value);
  void SetDateTime(const wxString& name, const wxDateTime& dateTime);

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