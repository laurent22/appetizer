#ifndef __UserSettings_H
#define __UserSettings_H

#include "boost/shared_ptr.hpp"
#include "utilities/XmlUtil.h"

class UserSettings {

public:

  UserSettings();

  int IconSize;
  wxString Locale;
  wxString PortableAppsPath;
  wxString DocumentsPath;
  wxString MusicPath;
  wxString PicturesPath;
  wxString VideosPath;

  void Save();
  void Load();

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

private:

  void AppendSettingToXml(TiXmlElement* element, const char* name, const char* value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, wxString value);
  void AssignSettingValue(int& setting, wxString value, int defaultValue);
  //void AssignSettingValue(wxString& setting, wxString value, wxString defaultValue);
  //void AssignSettingValue(wxString& setting, wxString value);
  void AssignSettingValue(int& setting, wxString value);

};

typedef boost::shared_ptr<UserSettings> UserSettingsSP;

#endif // __UserSettings_H