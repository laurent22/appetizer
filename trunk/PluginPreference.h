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
    Popup,
    CheckBox,
    File,
    Spinner,
    Hidden
  };
};


typedef std::map<wxString, wxString> PluginPreferenceOptions;


class PluginPreference {

public:

  PluginPreference(int type, const wxString& name, const wxString& defaultValue, const wxString& title, const wxString& description, PluginPreferenceGroup* group, PluginPreferenceOptions options);
  wxString GetName();
  wxString GetValue();
  double GetDoubleValue();
  int GetIntValue();
  bool GetBoolValue();
  wxString GetTitle();
  PluginPreferenceGroup* GetGroup();
  PluginPreferenceOptions GetOptions();
  int GetType();
  wxString GetDescription();
  wxString GetDefaultValue();
  double GetMinValue();
  double GetMaxValue();
  void SetRange(double minValue, double maxValue);
  void SetValue(const wxString& value);
  bool IsInvalidated();
  bool ValueHasBeenSet();
  void SetSecure(bool secure);
  bool IsSecure();
  void Validate();
  void Invalidate();
  bool IsBoolean();
  bool IsInteger();
  bool IsDouble();
  bool IsString();

private:

  wxString name_;
  wxString value_;
  wxString defaultValue_;
  wxString title_;
  bool secure_;
  wxString description_;
  PluginPreferenceGroup* group_;
  int type_;
  PluginPreferenceOptions options_;
  bool hasBeenSet_;
  bool invalidated_;
  double minValue_;
  double maxValue_;

};


typedef std::vector<PluginPreference*> PluginPreferenceVector;


#endif // __PluginPreference_H