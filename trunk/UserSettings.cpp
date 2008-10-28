#include "UserSettings.h"
#include <wx/fileconf.h>
#include "FilePaths.h"
#include "Controller.h"


extern Controller gController;


UserSettings::UserSettings() {
  IconSize = 16;
  Locale = _T("en");
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  AppendSettingToXml(xml, "IconSize", IconSize);
  AppendSettingToXml(xml, "Locale", Locale);

  return xml;
}


void UserSettings::FromXml(TiXmlElement* xml) {
  for (TiXmlElement* element = xml->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName != _T("Setting")) {
      wxLogDebug(_T("UserSettings::FromXml: Unknown element: %s"), elementName);
      continue;
    }

    const char* cSettingName = element->Attribute("name");
    if (!cSettingName) {
      wxLogDebug(_T("UserSettings::FromXml: setting doesn t have a name"));
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


void UserSettings::Load() {
  TiXmlDocument doc(FilePaths::SettingsFile.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Settings");
  if (!root) {
    wxLogDebug(_T("UserSettings::Load: Could not load XML. No Settings element found."));
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

  wxString filePath = FilePaths::SettingsFile;
  doc.SaveFile(filePath.mb_str());
}