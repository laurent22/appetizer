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
  Load();
}


PluginPreferences::~PluginPreferences() {
  for (int i = 0; i < preferences_.size(); i++) {
    PluginPreference* p = preferences_.at(i);
    wxDELETE(p);
  }
  preferences_.clear();
}


int PluginPreferences::Count() {
  return preferences_.size();
}


PluginPreference* PluginPreferences::GetPreferenceAt(int index) {
  return preferences_.at(index);
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
  
  TiXmlDocument doc(filePath_.mb_str());
  doc.LoadFile(TIXML_ENCODING_UTF8);
  TiXmlElement* preferencesXml = doc.FirstChildElement("PluginPreferences");
  if (!preferencesXml) WLOG(_T("PluginPreferences::Load: Could not load XML. No PluginPreferences element found."));

  for (TiXmlElement* element = preferencesXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    TiXmlHandle handle(element);

    wxString preferenceName = XmlUtil::ReadElementText(handle, "Name");
    wxString preferenceValue = XmlUtil::ReadElementText(handle, "Value");

    PluginPreference* preference = GetPreference(preferenceName);
    if (!preference) {
      // Fail silently - if the preference is set in the XML but 
      // hasn't been registered, it may mean that it is obsolete.
      // We just ignore it.
      continue;
    }

    preference->SetValue(preferenceValue);
  }

}