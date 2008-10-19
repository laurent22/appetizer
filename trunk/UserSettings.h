#ifndef __UserSettings_H
#define __UserSettings_H

#include "boost/shared_ptr.hpp"

class UserSettings {

public:

  UserSettings();
  int IconSize;

  void Save();
  void Load();

};

typedef boost::shared_ptr<UserSettings> UserSettingsSP;

#endif // __UserSettings_H