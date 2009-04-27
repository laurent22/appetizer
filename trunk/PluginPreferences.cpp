/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginPreferences.h"
#include "utilities/XmlUtil.h"
#include "FilePaths.h"


PluginPreferences::PluginPreferences(const wxString& filePath) {
  filePath_ = filePath;
}


PluginPreferences::~PluginPreferences() {
  for (int i = 0; i < preferences_.size(); i++) {
    PluginPreference* p = preferences_.at(i);
    wxDELETE(p);
  }
  preferences_.clear();
}


PluginPreference* PluginPreferences::RegisterPreference(PluginPreference* preference) {
  preferences_.push_back(preference);
  return preference;
}


PluginPreference* PluginPreferences::GetPreference(const wxString& name) {
  for (int i = 0; i < preferences_.size(); i++) {
    PluginPreference* p = preferences_.at(i);
    if (p->GetName() == name) return p;
  }
  return NULL;
}


void PluginPreferences::Save() {
  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "UTF-8", ""));

  TiXmlElement* xmlRoot = new TiXmlElement("PluginPreferences");
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  for (int i = 0; i < preferences_.size(); i++) {
    PluginPreference* preference = preferences_.at(i);
    if (!preference->ValueHasBeenSet()) continue;

    TiXmlElement* xmlPref = new TiXmlElement("Preference");
    xmlRoot->LinkEndChild(xmlPref);

    XmlUtil::AppendTextElement(xmlPref, "Name", preference->GetName());
    XmlUtil::AppendTextElement(xmlPref, "Value", preference->GetValue());
  }

  wxFileName f(filePath_);

  FilePaths::CreateDirectoryIfNotExists(f.GetPath());
  bool saved = doc.SaveFile(filePath_.mb_str());
  if (!saved) ELOG(_T("Could not save preference file: ") + filePath_);
}


void PluginPreferences::Load() {
  if (!wxFileName::FileExists(filePath_)) return;
  
  //TiXmlDocument doc(FilePaths::GetPluginSettingsFile().mb_str());
  //doc.LoadFile(TIXML_ENCODING_UTF8);
  //TiXmlElement* pluginSettingsXml = doc.FirstChildElement("Plugins");
  //if (!pluginSettingsXml) WLOG(_T("PluginManager::Initialize: Could not load XML. No Plugins element found."));


}