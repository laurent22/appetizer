/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "UserSettings.h"
#include <wx/fileconf.h>
#include <wx/datetime.h>
#include <wx/filename.h>
#include "Constants.h"
#include "Log.h"
#include "FilePaths.h"
#include "Controller.h"


extern Controller gController;


UserSettings::UserSettings() {
  IconSize = 32;
  Locale = _T("en");
  PortableAppsPath = _T("%DRIVE%/PortableApps");
  DocumentsPath = _T("%DRIVE%/Documents");
  MusicPath = _T("%DRIVE%/Documents/Music");
  PicturesPath = _T("%DRIVE%/Documents/Pictures");
  VideosPath = _T("%DRIVE%/Documents/Videos");
  Skin = _T("Default");
  Rotated = false;

  NextUpdateCheckTime = wxDateTime::Now();
  NextUpdateCheckTime.Add(wxTimeSpan(24 * CHECK_VERSION_DAY_INTERVAL));
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
  AppendSettingToXml(xml, "NextUpdateCheckTime", NextUpdateCheckTime.Format());

  return xml;
}


void UserSettings::FromXml(TiXmlElement* xml) {
  for (TiXmlElement* element = xml->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName != _T("Setting")) {
      wlog(wxString::Format(_T("UserSettings::FromXml: Unknown element: %s"), elementName));
      continue;
    }

    const char* cSettingName = element->Attribute("name");
    if (!cSettingName) {
      wlog("UserSettings::FromXml: setting doesn t have a name");
      continue;
    }

    const char* cSettingValue = element->GetText();
    
    wxString n = wxString(cSettingName, wxConvUTF8);
    wxString v;
    if (!cSettingValue) {
      v = wxEmptyString;
    } else {
      v = wxString(cSettingValue, wxConvUTF8);
    }

    if (n == _T("IconSize")) AssignSettingValue(IconSize, v);
    if (n == _T("Locale")) Locale = v;
    if (n == _T("PortableAppsPath")) PortableAppsPath = v;
    if (n == _T("DocumentsPath")) DocumentsPath = v;
    if (n == _T("MusicPath")) MusicPath = v;
    if (n == _T("PicturesPath")) PicturesPath = v;
    if (n == _T("VideosPath")) VideosPath = v;
    if (n == _T("Skin")) Skin = v;
    if (n == _T("Rotated")) Rotated = v.Lower() == _T("true");
    if (n == _T("NextUpdateCheckTime")) NextUpdateCheckTime.ParseFormat(v);

  }
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
  AppendSettingToXml(element, name, value.mb_str());
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, bool value) {
  AppendSettingToXml(element, name, value ? "true" : "false");
}


void UserSettings::Load() {
  TiXmlDocument doc(FilePaths::GetSettingsFile().mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Settings");
  if (!root) {
    wlog("UserSettings::Load: Could not load XML. No Settings element found.");
    return;
  }

  FromXml(root);
}


void UserSettings::Save() {
  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "", ""));
  TiXmlElement* xmlRoot = ToXml();
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  wxString filePath = FilePaths::GetSettingsFile();

  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(filePath.mb_str());
}