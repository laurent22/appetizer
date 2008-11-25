/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __UserSettings_H
#define __UserSettings_H

#include "utilities/XmlUtil.h"


class UserSettings {

public:

  UserSettings();

  int IconSize;
  wxString Locale;
  wxString PortableAppsPath;
  wxString DocumentsPath;
  wxString MusicPath;
  wxString PicturesPath;
  wxString VideosPath;
  wxString Skin;
  bool Rotated;
  wxDateTime NextUpdateCheckTime;
  bool AlwaysOnTop;
  bool AutoHideApplication;
  bool UniqueApplicationInstance;
  bool ShowDeleteIconMessage;
  bool ShowEjectDriveMessage;
  bool RunMultiLaunchOnStartUp;
  bool CloseAppsOnEject;
  bool HotKeyControl;
  bool HotKeyAlt;
  bool HotKeyShift;
  int HotKeyKey;

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  int GetValidatedIconSize();

private:

  void AppendSettingToXml(TiXmlElement* element, const char* name, const char* value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, wxString value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, bool value);
  void AssignSettingValue(int& setting, wxString value, int defaultValue);
  void AssignSettingValue(int& setting, wxString value);

  bool ParseBoolean(const wxString& toParse);

};

typedef boost::shared_ptr<UserSettings> UserSettingsSP;

#endif // __UserSettings_H