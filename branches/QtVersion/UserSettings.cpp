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


void UserSettings::destroyInstance() {
  if (!instance_) return;
  SAFE_DELETE(instance_);
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


UserSettings::~UserSettings() {
  UserSettingsMap::iterator i;
  for (i = values_.begin(); i != values_.end(); ++i) {
    SAFE_DELETE(i->second);
  }
  values_.clear();
}


QString UserSettings::GetString(const QString& name) {
  return values_[name]->value;
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


void UserSettings::SetValue(const QString& name, const QString& value, int type) {
  UserSetting* s;
  
  if (values_.find(name) == values_.end()) {
    s = new UserSetting();
  } else {
    s = values_[name];
  }
  
  s->value = value;
  s->type = type;
  values_[name] = s;
}


void UserSettings::SetString(const QString& name, const QString& value) {
  SetValue(name, value, Type_String);
}


void UserSettings::SetInt(const QString& name, int value) {
  SetValue(name, QString::number(value), Type_Int);
}


void UserSettings::SetBool(const QString& name, bool value) {
  SetValue(name, value ? "1" : "0", Type_Bool);
}


void UserSettings::SetDateTime(const QString& name, const QDate& dateTime) {
  SetValue(name, dateTime.toString(Qt::ISODate), Type_Date);
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  UserSettingsMap::iterator i;
  for (i = values_.begin(); i != values_.end(); ++i) {
    AppendSettingToXml_(xml, i->first.toUtf8(), i->second->value.toUtf8(), i->second->type);
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
    const char* cSettingType = element->Attribute("type");
    if (!cSettingName) {
      qWarning() << "UserSettings::FromXml: setting doesn't have a name";
      continue;
    }

    const char* cSettingValue = element->GetText();
    
    QString n = QString::fromUtf8(cSettingName);
    QString t = QString::fromUtf8(cSettingType);
    int typeInt = 0;
    if (t == "String") {
      typeInt = Type_String;
    } else if (t == "Int") {
      typeInt = Type_Int;
    } else if (t == "Bool") {
      typeInt = Type_Bool;
    } else if (t == "Date") {
      typeInt = Type_Date;
    } else if (t == "Color") {
      typeInt = Type_Color;
    }
    QString v;
    if (!cSettingValue) {
      v = "";
    } else {
      v = QString::fromUtf8(cSettingValue);
    }

    v = v.trimmed();

    SetValue(n, v, typeInt);

  }
}


void UserSettings::AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value, int type) {
  char* typeString = new char[256];
  if (type == Type_String) { typeString = "String\0"; }
  else if (type == Type_Int) { typeString = "Int\0"; }
  else if (type == Type_Bool) { typeString = "Bool\0"; }
  else if (type == Type_Date) { typeString = "Date\0"; }
  else if (type == Type_Color) { typeString = "Color\0"; }

  TiXmlElement* e = new TiXmlElement("Setting");
  e->SetAttribute("name", name);
  e->SetAttribute("type", type);
  TiXmlText* t = new TiXmlText(value);
  e->LinkEndChild(t);
  element->LinkEndChild(e);
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