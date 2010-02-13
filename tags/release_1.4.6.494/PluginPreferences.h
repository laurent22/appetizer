/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginPreferences_H
#define __PluginPreferences_H

#include "PluginPreference.h"
#include "Enumerations.h"


typedef std::vector<PluginPreferenceGroup*> PluginPreferenceGroupVector;


struct PluginPreferenceSavedData {
  wxString Name;
  wxString Value;
};

typedef std::vector<PluginPreferenceSavedData> PluginPreferenceSavedDataVector;


class PluginPreferences : public wxEvtHandler {

public:

  PluginPreferences(const wxString& filePath = wxEmptyString);
  ~PluginPreferences();
  void RegisterPreferenceGroup(PluginPreferenceGroup* preferenceGroup);
  void RegisterPreference(PluginPreference* preference);
  PluginPreference* GetPreference(const wxString& name);
  PluginPreference* GetPreferenceAt(int index);
  PluginPreferenceGroup* GetPreferenceGroup(const wxString& name);
  PluginPreferenceGroupVector GetPreferenceGroups();
  void OnTimer(wxTimerEvent& evt);
  int CountGroupPreferences(const wxString& groupName);
  int Count();
  void ScheduleSave();
  void Save();
  void Load();

private:

  PluginPreferenceGroupVector preferenceGroups_;
  PluginPreferenceVector preferences_;
  wxTimer* scheduledSaveTimer_;
  wxString filePath_;
  PluginPreferenceSavedDataVector savedData_;

  DECLARE_EVENT_TABLE()

};


#endif // __PluginPreferences_H