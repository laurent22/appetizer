/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <XmlUtil.h>
#include <UserSettings.h>
#include <Constants.h>
#include <FilePaths.h>

using namespace appetizer;


UserSettings* UserSettings::instance_ = NULL;


UserSettings* UserSettings::instance() {
  if (!instance_) instance_ = new UserSettings();
  return instance_;
}


UserSettings::UserSettings() {
  SetInt("IconSize", Application::instance()->getValidIconSize(LARGE_ICON_SIZE));
  SetString("Locale", "en");
  SetString("PortableAppsPath", "$(Drive/PortableApps");
  SetString("DocumentsPath", "$(Drive/Documents");
  SetString("MusicPath", "$(Drive/Documents/Music");
  SetString("PicturesPath", "$(Drive/Documents/Pictures");
  SetString("VideosPath", "$(Drive/Documents/Videos");
  SetString("Skin", "Default");
  SetBool("Rotated", false);
  SetBool("AlwaysOnTop", false);
  SetBool("MinimizeOnClose", true);
  SetBool("UniqueApplicationInstance", true);
  SetBool("AutoHideApplication", false);
  SetBool("RunMultiLaunchOnStartUp", false);
  SetBool("CloseAppsOnEject", false);
  SetBool("HotKeyControl", false);
  SetBool("HotKeyAlt", false);
  SetBool("HotKeyShift", false);
  SetInt("HotKeyKey", 0);
  SetBool("LaunchAppHotKeyControl", false);
  SetBool("LaunchAppHotKeyAlt", false);
  SetBool("LaunchAppHotKeyShift", false);
  SetInt("LaunchAppHotKeyKey", 0);
  SetInt("WindowTransparency", 235);
  SetBool("OptionPanelOpen", true);
  SetBool("TaskBarIcon", false);
  SetBool("TrayIcon", true);
  SetBool("IconLabelPosition", "bottom");

  SetBool("ShowDeleteIconMessage", true);
  SetBool("ShowEjectDriveMessage", true);
  SetBool("ShowMinimizeMessage", true);

  //QDate now = QDate::Now();
  //// This is just to force an update check the first time the app is launched.
  //now.Subtract(wxTimeSpan(24));
  //SetDateTime("NextUpdateCheckTime", now);
}


QString UserSettings::GetString(const QString& name) {
  return values_[name];
}


int UserSettings::GetInt(const QString& name) {
  QString s = GetString(name);

  bool ok = false;
  int output = s.toInt(&ok);
  if (!ok) {
    qCritical() << "Cannot convert value '" << s << "' of property '" << name << "' to int.";
    output = 0;
  }

  return output;
}


bool UserSettings::GetBool(const QString& name) {
  QString s = GetString(name);

  if (s == "0") return false;
  if (s == "1") return true;

  s = s.toLower();

  return s == "true";
}


QDate UserSettings::GetDateTime(const QString& name) {
  return QDate::fromString(GetString(name), Qt::ISODate);
}


void UserSettings::SetString(const QString& name, const QString& value) {
  values_[name] = QString(value);
}


void UserSettings::SetInt(const QString& name, int value) {
  SetString(name, QString::number(value));
}


void UserSettings::SetBool(const QString& name, bool value) {
  SetString(name, value ? "1" : "0");
}


void UserSettings::SetDateTime(const QString& name, const QDate& dateTime) {
  SetString(name, dateTime.toString(Qt::ISODate));
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  UserSettingsMap::iterator i;
  for (i = values_.begin(); i != values_.end(); ++i) {
    AppendSettingToXml(xml, i->first.toUtf8(), i->second);
  }

  return xml;
}


int UserSettings::GetValidatedIconSize(int iconSize) {
  return Application::instance()->getValidIconSize(iconSize > 0 ? iconSize : GetInt("IconSize"));
}


void UserSettings::FromXml(TiXmlElement* xml) {
  for (TiXmlElement* element = xml->FirstChildElement(); element; element = element->NextSiblingElement()) {
    QString elementName = QString::fromUtf8(element->Value());

    if (elementName != "Setting") {
      qWarning() << "UserSettings::FromXml: Unknown element:" << elementName;
      continue;
    }

    const char* cSettingName = element->Attribute("name");
    if (!cSettingName) {
      qWarning() << "UserSettings::FromXml: setting doesn't have a name";
      continue;
    }

    const char* cSettingValue = element->GetText();
    
    QString n = QString::fromUtf8(cSettingName);
    QString v;
    if (!cSettingValue) {
      v = "";
    } else {
      v = QString::fromUtf8(cSettingValue);
    }

    v = v.trimmed();

    SetString(n, v);

  }
}


bool UserSettings::ParseBoolean(const QString& toParse) {
  QString t = toParse.toLower();
  return t == "true" || t == "1";
}


void UserSettings::AssignSettingValue(int& setting, QString value, int defaultValue) {
  bool ok = false;
  int tempValue = value.toInt(&ok);

  if(!ok) {
    setting = defaultValue;
  } else {
    setting = tempValue;
  }
}


void UserSettings::AssignSettingValue(int& setting, QString value) {
  bool ok = false;
  int tempValue = value.toInt(&ok);
  if(!ok) return;
  setting = tempValue;
}


void UserSettings::AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value) {
  TiXmlElement* e = new TiXmlElement("Setting");
  e->SetAttribute("name", name);
  TiXmlText* t = new TiXmlText(value);
  e->LinkEndChild(t);
  element->LinkEndChild(e);
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, int value) {
  AppendSettingToXml(element, name, QString::number(value));
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, QString value) {
  AppendSettingToXml_(element, name, value.toUtf8());
}


void UserSettings::AppendSettingToXml(TiXmlElement* element, const char* name, bool value) {
  AppendSettingToXml_(element, name, value ? "1" : "0");
}


void UserSettings::Load() {
  QString filePath = FilePaths::GetSettingsFile();
  TiXmlDocument doc(filePath.toUtf8());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("Settings");
  if (!root) {
    qWarning() << "UserSettings::Load: Could not load XML. No Settings element found.";
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

  QString filePath = FilePaths::GetSettingsFile();
  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(filePath.toUtf8());
}