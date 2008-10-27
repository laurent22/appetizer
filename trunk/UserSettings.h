#ifndef __UserSettings_H
#define __UserSettings_H

#include "boost/shared_ptr.hpp"
#include "utilities/XmlUtil.h"

class UserSettings {

public:

  UserSettings();
  int IconSize;
  void Save();

  TiXmlElement* ToXml();

private:

  void AppendSettingToXml(TiXmlElement* element, const char* name, const char* value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, int value);
  void AppendSettingToXml(TiXmlElement* element, const char* name, wxString value);

};

typedef boost::shared_ptr<UserSettings> UserSettingsSP;

#endif // __UserSettings_H