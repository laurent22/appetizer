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
#include <gettext_lib/QtLocaleUtil.h>

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
  group_ = _("General");
  controlType_ = Undefined;
  minValue_ = 0;
  maxValue_ = 100;
}


UserSetting::ControlType UserSetting::controlType() const {
  if (controlType_ != Undefined) return controlType_;
  
  QVariant::Type type = value().type();
  
  if (type == QVariant::String) return TextBox;
  if (type == QVariant::Int) return SpinBox;
  if (type == QVariant::Bool) return CheckBox;

  return TextBox;
}


void UserSetting::setValue(const QVariant& value) {
  if (value == value_) return;
  value_ = value;
  emit valueChanged();
}


void UserSetting::setLabel(const QString& label) {
  label_ = label;
}


void UserSetting::setGroup(const QString& group) {
  group_ = group;
}


void UserSetting::setControlType(UserSetting::ControlType type) {
  controlType_ = type;
}


void UserSetting::setOptions(const UserSettingOptions& options) {
  options_ = options;
}


void UserSetting::setMinValue(int v) {
  minValue_ = v;
}


void UserSetting::setMaxValue(int v) {
  maxValue_ = v;
}


UserSettings::UserSettings() {
  UserSetting* s = NULL;

  // =================================================================
  // GENERAL
  // =================================================================

  s = setString("Locale", "en");
  s->setLabel(_("Language"));
  s->setControlType(UserSetting::ComboBox);

  QStringList locales = QtGettext::instance()->availableLocales();
  UserSettingOptions options;
  QtLocaleUtil localeUtil;
  for (int i = 0; i < locales.size(); i++) {
    QString localeCode = locales[i];
    std::pair<QString, QString> option(localeUtil.getDisplayName(localeCode), localeCode);
    options.push_back(option);
  }
  s->setOptions(options);

  s = setBool("AlwaysOnTop", false);
  s->setLabel(_("Always on top"));

  s = setBool("UniqueApplicationInstance", true);
  s->setLabel(StringUtil::convertFields(_("Allow only one instance of %s at a time")).arg(APPLICATION_NAME));

  s = setBool("TaskBarIcon", false);
  s->setLabel(_("Show taskbar icon"));

  s = setBool("TrayIcon", true);
  s->setLabel(_("Show tray icon"));

  
  // =================================================================
  // APPEARANCE
  // =================================================================

  s = setString("Skin", "Default");
  s->setLabel(_("Skin:"));
  s->setGroup(_("Appearance"));

  s = setInt("IconSize", Application::instance()->getValidIconSize(LARGE_ICON_SIZE));
  s->setLabel(_("Icon size:"));
  s->setGroup(_("Appearance"));
  s->setMinValue(1);
  s->setMaxValue(512);

  s = setInt("WindowTransparency", 235);
  s->setLabel(_("Opacity:"));
  s->setGroup(_("Appearance"));

  s = setString("IconLabelPosition", "bottom");
  s->setControlType(UserSetting::ComboBox);
  s->setLabel(_("Label position:"));
  s->setGroup(_("Appearance"));


  // =================================================================
  // OPERATIONS
  // =================================================================

  s = setBool("AutoHideApplication", false);
  s->setLabel(_("Auto-hide after launching an application"));
  s->setGroup(_("Operations"));

  s = setBool("CloseAppsOnEject", false);
  s->setLabel(_("When ejecting the drive, close the apps that are locking it"));
  s->setGroup(_("Operations"));
  
  
  // =================================================================
  // HIDDEN
  // =================================================================

  s = setBool("ShowDeleteIconMessage", true);
  s = setBool("ShowEjectDriveMessage", true);
  s = setBool("ShowMinimizeMessage", true);
  s = setString("PortableAppsPath", "$(Drive/PortableApps");
  s = setString("DocumentsPath", "$(Drive/Documents");
  s = setString("MusicPath", "$(Drive/Documents/Music");
  s = setString("PicturesPath", "$(Drive/Documents/Pictures");
  s = setString("VideosPath", "$(Drive/Documents/Videos");
  s = setBool("OptionPanelOpen", true);

  QDateTime now = QDateTime::currentDateTime();
  now.addMonths(-1); // This is just to force an update check the first time the app is launched.
  s = setDateTime("NextUpdateCheckTime", now);
}


UserSettings::~UserSettings() {
  UserSettingsMap::iterator i;
  for (i = settings_.begin(); i != settings_.end(); ++i) {
    SAFE_DELETE(i->second);
  }
  settings_.clear();
}


std::vector<QString> UserSettings::getGroupLabels() const {
  std::vector<QString> output;

  for (UserSettingsMap::iterator i = settings_.begin(); i != settings_.end(); ++i) {
    UserSetting* s = i->second;
    if (VectorUtil::getElementIndex(output, s->group()) >= 0) continue;
    output.push_back(s->group());
  }

  return output;
}


QString UserSettings::getGroupLabelAt(int index) const {
  return "";
}


UserSettingsVector UserSettings::getSettingsByGroup(const QString& groupName) {
  UserSettingsVector output;

  UserSettingsMap::iterator i;
  for (i = settings_.begin(); i != settings_.end(); ++i) {
    UserSetting* s = i->second;
    if (s->group() == groupName) output.push_back(s);
  }

  return output;
}


TiXmlElement* UserSettings::ToXml() {
  TiXmlElement* xml = new TiXmlElement("Settings");

  //UserSettingsMap::iterator i;
  //for (i = values_.begin(); i != values_.end(); ++i) {
  //  AppendSettingToXml_(xml, i->first.toUtf8(), i->second->value.toUtf8(), i->second->type);
  //}

  return xml;
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


UserSetting* UserSettings::setSetting(const QString& name, const QVariant& variant) {
  UserSetting* setting = NULL;
  
  bool isNew = false;
  
  if (settings_.find(name) != settings_.end()) {
    setting = settings_[name];
  } else {
    setting = new UserSetting(name);
    settings_[name] = setting;
    isNew = true;
  }

  setting->setValue(variant);

  if (isNew) QObject::connect(setting, SIGNAL(valueChanged()), this, SLOT(setting_valueChanged()));

  return setting;
}


void UserSettings::setting_valueChanged() {
  UserSetting* setting = static_cast<UserSetting*>(QObject::sender());
  qDebug() << "Setting changed:" << setting->name() << ":" << setting->value();
  emit settingChanged(setting);
}


UserSetting* UserSettings::setInt(const QString& name, int value) { return setSetting(name, QVariant(value)); }
UserSetting* UserSettings::setString(const QString& name, const QString& value) { return setSetting(name, QVariant(value)); }
UserSetting* UserSettings::setBool(const QString& name, bool value) { return setSetting(name, QVariant(value)); }
UserSetting* UserSettings::setDateTime(const QString& name, const QDateTime& value) { return setSetting(name, QVariant(value)); }
UserSetting* UserSettings::setColor(const QString& name, const QColor& value) { return setSetting(name, value); }


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