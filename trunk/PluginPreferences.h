/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginPreferences_H
#define __PluginPreferences_H

#include "PluginPreference.h"





class PluginPreferences {

public:

  PluginPreferences(const wxString& filePath);
  ~PluginPreferences();
  PluginPreference* RegisterPreference(PluginPreference* preference);
  PluginPreference* GetPreference(const wxString& name);
  int Count();
  void Save();
  void Load();

private:

  PluginPreferenceVector preferences_;
  wxString filePath_;

};


#endif // __PluginPreferences_H