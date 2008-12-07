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
  IconSize = wxGetApp().GetOSValidIconSize(LARGE_ICON_SIZE);
  Locale = _T("en");
  PortableAppsPath = _T("$(Drive)/PortableApps");
  DocumentsPath = _T("$(Drive)/Documents");
  MusicPath = _T("$(Drive)/Documents/Music");
  PicturesPath = _T("$(Drive)/Documents/Pictures");
  VideosPath = _T("$(Drive)/Documents/Videos");
  Skin = _T("Default");
  Rotated = false;
  AlwaysOnTop = false;
  MinimizeOnClose = true;
  UniqueApplicationInstance = true;
  AutoHideApplication = false;
  RunMultiLaunchOnStartUp = false;
  CloseAppsOnEject = false;
  HotKeyControl = false;
  HotKeyAlt = false;
  HotKeyShift = false;
  HotKeyKey = 0;

  ShowDeleteIconMessage = true;
  ShowEjectDriveMessage = true;
  ShowMinimizeMessage = true;

  NextUpdateCheckTime = wxDateTime::Now();
  // This is just to force an update check the first time the 
  // app is launched.
  NextUpdateCheckTime.Subtract(wxTimeSpan(24));
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  AppendSettingToXml(xml, "IconSize", IconSize);
  AppendSettingToXml(xml, "Locale", Locale);
  AppendSettingToXml(xml, "PortableAppsPath", PortableAppsPath);
  AppendSettingToXml(xml, "DocumentsPath", DocumentsPath);
  AppendSettingToXml(xml, "MusicPath", MusicPath);
  AppendSettingToXml(xml, "PicturesPath", PicturesPath);
  AppendSettingToXml(xml, "VideosPath", VideosPath);
  AppendSettingToXml(xml, "Skin", Skin);
  AppendSettingToXml(xml, "Rotated", Rotated);
  AppendSettingToXml(xml, "NextUpdateCheckTime", NextUpdateCheckTime.Format(ISO_DATE_FORMAT));
  AppendSettingToXml(xml, "AlwaysOnTop", AlwaysOnTop);
  AppendSettingToXml(xml, "AutoHideApplication", AutoHideApplication);
  AppendSettingToXml(xml, "UniqueApplicationInstance", UniqueApplicationInstance);
  AppendSettingToXml(xml, "ShowDeleteIconMessage", ShowDeleteIconMessage);
  AppendSettingToXml(xml, "ShowEjectDriveMessage", ShowEjectDriveMessage);
  AppendSettingToXml(xml, "ShowMinimizeMessage", ShowMinimizeMessage);
  AppendSettingToXml(xml, "MinimizeOnClose", MinimizeOnClose);
  AppendSettingToXml(xml, "RunMultiLaunchOnStartUp", RunMultiLaunchOnStartUp);
  AppendSettingToXml(xml, "HotKeyControl", HotKeyControl);
  AppendSettingToXml(xml, "HotKeyAlt", HotKeyAlt);
  AppendSettingToXml(xml, "HotKeyShift", HotKeyShift);
  AppendSettingToXml(xml, "HotKeyKey", HotKeyKey);
  AppendSettingToXml(xml, "CloseAppsOnEject", CloseAppsOnEject);

  return xml;
}


int UserSettings::GetValidatedIconSize() {
  return wxGetApp().GetOSValidIconSize(IconSize);
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

    if (n == _T("IconSize")) AssignSettingValue(IconSize, v);
    if (n == _T("Locale")) Locale = v;
    if (n == _T("PortableAppsPath")) PortableAppsPath = v;
    if (n == _T("DocumentsPath")) DocumentsPath = v;
    if (n == _T("MusicPath")) MusicPath = v;
    if (n == _T("PicturesPath")) PicturesPath = v;
    if (n == _T("VideosPath")) VideosPath = v;
    if (n == _T("Skin")) Skin = v;
    if (n == _T("Rotated")) Rotated = ParseBoolean(v);
    if (n == _T("NextUpdateCheckTime")) {
      const wxChar* c = NextUpdateCheckTime.ParseFormat(v, ISO_DATE_FORMAT);
      if (!c) NextUpdateCheckTime = wxDateTime::Now();
    }
    if (n == _T("AlwaysOnTop")) AlwaysOnTop = ParseBoolean(v);
    if (n == _T("AutoHideApplication")) AutoHideApplication = ParseBoolean(v);
    if (n == _T("UniqueApplicationInstance")) UniqueApplicationInstance = ParseBoolean(v);
    if (n == _T("ShowDeleteIconMessage")) ShowDeleteIconMessage = ParseBoolean(v);
    if (n == _T("ShowEjectDriveMessage")) ShowEjectDriveMessage = ParseBoolean(v);
    if (n == _T("RunMultiLaunchOnStartUp")) RunMultiLaunchOnStartUp = ParseBoolean(v);
    if (n == _T("HotKeyControl")) HotKeyControl = ParseBoolean(v);
    if (n == _T("HotKeyAlt")) HotKeyAlt = ParseBoolean(v);
    if (n == _T("HotKeyShift")) HotKeyShift = ParseBoolean(v);
    if (n == _T("HotKeyKey")) AssignSettingValue(HotKeyKey, v);
    if (n == _T("CloseAppsOnEject")) CloseAppsOnEject = ParseBoolean(v);
    if (n == _T("MinimizeOnClose")) MinimizeOnClose = ParseBoolean(v);
    if (n == _T("ShowMinimizeMessage")) ShowMinimizeMessage = ParseBoolean(v);

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