/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"


#ifndef __PluginPreference_H
#define __PluginPreference_H



class PluginPreferenceGroup { public:
  wxString Name;
  wxString Title;
};


struct PluginPreferenceType {
  enum {
    Text,
    TextArea,
    Popup
  };
};


typedef std::map<wxString, wxString> PluginPreferenceOptions;


class PluginPreference {

public:

  PluginPreference(int type, const wxString& name, const wxString& defaultValue, const wxString& title, const wxString& description, PluginPreferenceGroup* group, PluginPreferenceOptions options);
  wxString GetName();
  wxString GetValue();
  wxString GetTitle();
  PluginPreferenceGroup* GetGroup();
  PluginPreferenceOptions GetOptions();
  int GetType();
  wxString GetDescription();
  wxString GetDefaultValue();
  void SetValue(const wxString& value);
  bool IsInvalidated();
  bool ValueHasBeenSet();
  void Validate();
  void Invalidate();

private:

  wxString name_;
  wxString value_;
  wxString defaultValue_;
  wxString title_;
  wxString description_;
  PluginPreferenceGroup* group_;
  int type_;
  PluginPreferenceOptions options_;
  bool hasBeenSet_;
  bool invalidated_;

};


typedef std::vector<PluginPreference*> PluginPreferenceVector;


#endif // __PluginPreference_H