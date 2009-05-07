/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginPreferences_H
#define __PluginPreferences_H

#include "PluginPreference.h"


typedef std::vector<PluginPreferenceGroup*> PluginPreferenceGroupVector;





class PluginPreferences {

public:

  PluginPreferences(const wxString& filePath);
  ~PluginPreferences();
  void RegisterPreferenceGroup(PluginPreferenceGroup* preferenceGroup);
  void RegisterPreference(PluginPreference* preference);
  PluginPreference* GetPreference(const wxString& name);
  PluginPreference* GetPreferenceAt(int index);
  PluginPreferenceGroup* GetPreferenceGroup(const wxString& name);
  PluginPreferenceGroupVector GetPreferenceGroups();
  int CountGroupPreferences(const wxString& groupName);
  int Count();
  void Save();
  void Load();

private:

  PluginPreferenceGroupVector preferenceGroups_;
  PluginPreferenceVector preferences_;
  wxString filePath_;

};


#endif // __PluginPreferences_H