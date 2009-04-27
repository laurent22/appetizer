/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginPreference.h"


PluginPreference::PluginPreference(int type, const wxString& name, const wxString& defaultValue) {
  name_ = name;
  type_ = type;
  defaultValue_ = defaultValue;
  invalidated_ = false;
  hasBeenSet_ = false;
}


wxString PluginPreference::GetName() {
  return name_;
}


wxString PluginPreference::GetValue() {
  if (hasBeenSet_) return value_;
  return GetDefaultValue();
}


void PluginPreference::SetValue(const wxString& value) {
  if (value_ == value) return;
  hasBeenSet_ = true;
  value_ = value;
  Invalidate();
}


wxString PluginPreference::GetDefaultValue() {
  return defaultValue_;
}


void PluginPreference::Invalidate() {
  invalidated_ = true;
}


bool PluginPreference::IsInvalidated() {
  return invalidated_;
}


bool PluginPreference::ValueHasBeenSet() {
  return hasBeenSet_;
}