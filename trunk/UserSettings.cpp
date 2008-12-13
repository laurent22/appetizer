/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "UserSettings.h"
#include "Constants.h"
#include "FilePaths.h"
#include "MiniLaunchBar.h"


UserSettings::UserSettings() {
  SetInt(_T("IconSize"), wxGetApp().GetOSValidIconSize(LARGE_ICON_SIZE));
  SetString(_T("Locale"), _T("en"));
  SetString(_T("PortableAppsPath"), _T("$(Drive)/PortableApps"));
  SetString(_T("DocumentsPath"), _T("$(Drive)/Documents"));
  SetString(_T("MusicPath"), _T("$(Drive)/Documents/Music"));
  SetString(_T("PicturesPath"), _T("$(Drive)/Documents/Pictures"));
  SetString(_T("VideosPath"), _T("$(Drive)/Documents/Videos"));
  SetString(_T("Skin"), _T("Default"));
  SetBool(_T("Rotated"), false);
  SetBool(_T("AlwaysOnTop"), false);
  SetBool(_T("MinimizeOnClose"), true);
  SetBool(_T("UniqueApplicationInstance"), true);
  SetBool(_T("AutoHideApplication"), false);
  SetBool(_T("RunMultiLaunchOnStartUp"), false);
  SetBool(_T("CloseAppsOnEject"), false);
  SetBool(_T("HotKeyControl"), false);
  SetBool(_T("HotKeyAlt"), false);
  SetBool(_T("HotKeyShift"), false);
  SetInt(_T("HotKeyKey"), 0);

  SetBool(_T("ShowDeleteIconMessage"), true);
  SetBool(_T("ShowEjectDriveMessage"), true);
  SetBool(_T("ShowMinimizeMessage"), true);

  wxDateTime now = wxDateTime::Now();
  // This is just to force an update check the first time the app is launched.
  now.Subtract(wxTimeSpan(24));
  SetDateTime(_T("NextUpdateCheckTime"), now);
}


wxString UserSettings::GetString(const wxString& name) {
  return values_[name];
}


int UserSettings::GetInt(const wxString& name) {
  wxString s = GetString(name);

  long l;
  if (!s.ToLong(&l)) {
    ELOG(_T("Cannot convert value '%s' of property '%s' to int."), s, name);
    l = 0;
  }

  return (int)l;
}


bool UserSettings::GetBool(const wxString& name) {
  wxString s = GetString(name);

  if (s == _T("0")) return false;
  if (s == _T("1")) return true;

  s = s.Lower();

  return s == _T("true");
}


wxDateTime UserSettings::GetDateTime(const wxString& name) {
  wxDateTime output;

  const wxChar* c = output.ParseFormat(GetString(name), ISO_DATE_FORMAT);
  if (!c) output = wxDateTime::Now();

  return output;
}


void UserSettings::SetString(const wxString& name, const wxString& value) {
  values_[name] = wxString(value);
}


void UserSettings::SetInt(const wxString& name, int value) {
  SetString(name, wxString::Format(_T("%i"), value));
}


void UserSettings::SetBool(const wxString& name, bool value) {
  SetString(name, value ? _T("1") : _T("0"));
}


void UserSettings::SetDateTime(const wxString& name, const wxDateTime& dateTime) {
  SetString(name, dateTime.Format(ISO_DATE_FORMAT));
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  UserSettingsMap::iterator i;
  for (i = values_.begin(); i != values_.end(); ++i) {
    AppendSettingToXml(xml, i->first.ToUTF8(), i->second);
  }

  return xml;
}


int UserSettings::GetValidatedIconSize() {
  return wxGetApp().GetOSValidIconSize(GetInt(_T("IconSize")));
}


void UserSettings::FromXml(TiXmlElement* xml) {
  for (TiXmlElement* element = xml->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName != _T("Setting")) {
      WLOG(wxString::Format(_T("UserSettings::FromXml: Unknown element: %s"), elementName));
      continue;
    }

    const char* cSettingName = element->Attribute("name");
    if (!cSettingName) {
      WLOG(_T("UserSettings::FromXml: setting doesn't have a name"));
      continue;
    }

    const char* cSettingValue = element->GetText();
    
    wxString n = wxString::FromUTF8(cSettingName);
    wxString v;
    if (!cSettingValue) {
      v = wxEmptyString;
    } else {
      v = wxString::FromUTF8(cSettingValue);
    }

    v.Trim(true).Trim(false);

    SetString(n, v);

  }

}


bool UserSettings::ParseBoolean(const wxString& toParse) {
  wxString t = toParse.Lower();
  return t == _T("true") || t == _T("1");
}


void UserSettings::AssignSettingValue(int& setting, wxString value, int defaultValue) {
  long tempValue;
  if(!value.ToLong(&tempValue)) {
    setting = defaultValue;
  } else {
    setting = (int)tempValue;
  }
}


void UserSettings::AssignSettingValue(int& setting, wxString value) {
  long tempValue;
  if(!value.ToLong(&tempValue)) return;
  setting = (int)tempValue;
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, const char* value) {
  TiXmlElement* e = new TiXmlElement("Setting");
  e->SetAttribute("name", name);
  TiXmlText* t = new TiXmlText(value);
  e->LinkEndChild(t);
  element->LinkEndChild(e);
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, int value) {
  wxString s;
  s << value;
  AppendSettingToXml(element, name, s);
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, wxString value) {
  AppendSettingToXml(element, name, value.ToUTF8());
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, bool value) {
  AppendSettingToXml(element, name, value ? "1" : "0");
}


void UserSettings::Load() {
  TiXmlDocument doc(FilePaths::GetSettingsFile().mb_str());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("Settings");
  if (!root) {
    WLOG(_T("UserSettings::Load: Could not load XML. No Settings element found."));
    return;
  }

  FromXml(root);
}


void UserSettings::Save() {
  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "UTF-8", ""));
  TiXmlElement* xmlRoot = ToXml();
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  wxString filePath = FilePaths::GetSettingsFile();

  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(filePath.mb_str());
}