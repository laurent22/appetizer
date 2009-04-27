/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginPreference_H
#define __PluginPreference_H



struct PluginPreferenceType {
  enum {
    Text,
    TextArea,
    Popup
  };
};


class PluginPreference {

public:

  PluginPreference(int type, const wxString& name, const wxString& defaultValue);
  wxString GetName();
  wxString GetValue();
  wxString GetDefaultValue();
  void SetValue(const wxString& value);
  bool IsInvalidated();
  bool ValueHasBeenSet();

private:

  wxString name_;
  wxString value_;
  wxString defaultValue_;
  int type_;
  bool hasBeenSet_;
  bool invalidated_;
  void Invalidate();

};


typedef std::vector<PluginPreference*> PluginPreferenceVector;


#endif // __PluginPreference_H