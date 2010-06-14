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



UserSetting::UserSetting(const QString& name) {
  name_ = name;
}


void UserSetting::setValue(const QVariant& value) {
  value_ = value;
}


UserSettings::UserSettings() {
  setInt("IconSize", Application::instance()->getValidIconSize(LARGE_ICON_SIZE));
  setString("Locale", "en");
  setString("PortableAppsPath", "$(Drive/PortableApps");
  setString("DocumentsPath", "$(Drive/Documents");
  setString("MusicPath", "$(Drive/Documents/Music");
  setString("PicturesPath", "$(Drive/Documents/Pictures");
  setString("VideosPath", "$(Drive/Documents/Videos");
  setString("Skin", "Default");
  setBool("Rotated", false);
  setBool("AlwaysOnTop", false);
  setBool("MinimizeOnClose", true);
  setBool("UniqueApplicationInstance", true);
  setBool("AutoHideApplication", false);
  setBool("RunMultiLaunchOnStartUp", false);
  setBool("CloseAppsOnEject", false);
  setBool("HotKeyControl", false);
  setBool("HotKeyAlt", false);
  setBool("HotKeyShift", false);
  setInt("HotKeyKey", 0);
  setBool("LaunchAppHotKeyControl", false);
  setBool("LaunchAppHotKeyAlt", false);
  setBool("LaunchAppHotKeyShift", false);
  setInt("LaunchAppHotKeyKey", 0);
  setInt("WindowTransparency", 235);
  setBool("OptionPanelOpen", true);
  setBool("TaskBarIcon", false);
  setBool("TrayIcon", true);
  setBool("IconLabelPosition", "bottom");
  setBool("ShowDeleteIconMessage", true);
  setBool("ShowEjectDriveMessage", true);
  setBool("ShowMinimizeMessage", true);

  QDateTime now = QDateTime::currentDateTime();
  now.addMonths(-1); // This is just to force an update check the first time the app is launched.
  setDateTime("NextUpdateCheckTime", now);
}


UserSettings::~UserSettings() {
  UserSettingsMap::iterator i;
  for (i = settings_.begin(); i != settings_.end(); ++i) {
    SAFE_DELETE(i->second);
  }
  settings_.clear();
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  //UserSettingsMap::iterator i;
  //for (i = values_.begin(); i != values_.end(); ++i) {
  //  AppendSettingToXml_(xml, i->first.toUtf8(), i->second->value.toUtf8(), i->second->type);
  //}

  return xml;
}


int UserSettings::GetValidatedIconSize(int iconSize) {
  return 16;//Application::instance()->getValidIconSize(iconSize > 0 ? iconSize : GetInt("IconSize"));
}


QVariant::Type UserSettings::getSettingType(const QString& name) const {
  UserSetting* setting = getSetting(name);
  if (!setting) return QVariant::Invalid;
  return setting->value().type();
}


UserSetting* UserSettings::getSetting(const QString& name) const {
  if (settings_.find(name) != settings_.end()) return settings_[name];
  return NULL;
}


void UserSettings::setSetting(const QString& name, const QVariant& variant) {
  UserSetting* setting = NULL;
  
  if (settings_.find(name) != settings_.end()) {
    setting = settings_[name];
  } else {
    setting = new UserSetting(name);
    settings_[name] = setting;
  }

  setting->setValue(variant);
}


void UserSettings::setInt(const QString& name, int value) { setSetting(name, QVariant(value)); }
void UserSettings::setString(const QString& name, const QString& value) { setSetting(name, QVariant(value)); }
void UserSettings::setBool(const QString& name, bool value) { setSetting(name, QVariant(value)); }
void UserSettings::setDateTime(const QString& name, const QDateTime& value) { setSetting(name, QVariant(value)); }
void UserSettings::setColor(const QString& name, const QColor& value) { setSetting(name, value); }


int UserSettings::getInt(const QString& name) const {
  UserSetting* s = getSetting(name);
  return s ? s->value().toInt() : 0;
}


QString UserSettings::getString(const QString& name) const {
  UserSetting* s = getSetting(name);
  return s ? s->value().toString() : "";
}


bool UserSettings::getBool(const QString& name) const {
  UserSetting* s = getSetting(name);
  return s ? s->value().toBool() : false;
}


QDateTime UserSettings::getDateTime(const QString& name) const {
  UserSetting* s = getSetting(name);
  return s ? s->value().toDateTime() : QDateTime::currentDateTime();
}


QColor UserSettings::getColor(const QString& name) const {
  UserSetting* s = getSetting(name);
  return s ? s->value().value<QColor>() : QColor(Qt::red);
}


QString UserSettings::typeToString(QVariant::Type type) const {
  if (type == QVariant::String) return "String";
  if (type == QVariant::Bool) return "Bool";
  if (type == QVariant::Color) return "Color";
  if (type == QVariant::Int) return "Int";
  if (type == QVariant::DateTime) return "DateTime";
  return "String";
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
    QString v;
    if (!cSettingValue) {
      v = "";
    } else {
      v = QString::fromUtf8(cSettingValue);
      v = v.trimmed();
    }

    if (t == "") t = typeToString(getSettingType(n));

    int typeInt = 0;
    if (t == "String") {
      setString(n, v);
    } else if (t == "Int") {
      setInt(n, v.toInt());
    } else if (t == "Bool") {
      setBool(n, v == "1" || v.toLower() == "true");
    } else if (t == "DateTime") {
      setDateTime(n, QDateTime::fromString(v));
    } else if (t == "Color") {
      QStringList splitted = v.split(",");
      int r = 0;
      int g = 0;
      int b = 0;
      int a = 0;
      if (splitted.length() > 0) r = splitted[0].toInt();
      if (splitted.length() > 1) g = splitted[1].toInt();
      if (splitted.length() > 2) b = splitted[2].toInt();
      if (splitted.length() > 3) a = splitted[3].toInt();
      setColor(n, QColor(r, g, b, a));
    }
  }
}


void UserSettings::AppendSettingToXml_(TiXmlElement* element, const char* name, const char* value, int type) {
  //char* typeString = new char[256];
  //if (type == Type_String) { typeString = "String\0"; }
  //else if (type == Type_Int) { typeString = "Int\0"; }
  //else if (type == Type_Bool) { typeString = "Bool\0"; }
  //else if (type == Type_Date) { typeString = "Date\0"; }
  //else if (type == Type_Color) { typeString = "Color\0"; }

  //TiXmlElement* e = new TiXmlElement("Setting");
  //e->SetAttribute("name", name);
  //e->SetAttribute("type", type);
  //TiXmlText* t = new TiXmlText(value);
  //e->LinkEndChild(t);
  //element->LinkEndChild(e);
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
  xmlRoot->SetAttribute("version", "2.0");
  doc.LinkEndChild(xmlRoot);

  QString filePath = FilePaths::GetSettingsFile();
  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(filePath.toUtf8());
}