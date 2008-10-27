#include "UserSettings.h"
#include <wx/fileconf.h>
#include "FilePaths.h"


#include "Controller.h"
extern Controller gController;


UserSettings::UserSettings() {
  IconSize = 32;
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  AppendSettingToXml(xml, "IconSize", IconSize);

  return xml;
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


void UserSettings::Save() {
  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "", ""));
  TiXmlElement* xmlRoot = ToXml();
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  wxString filePath = FilePaths::SettingsFile;
  doc.SaveFile(filePath.mb_str());
}