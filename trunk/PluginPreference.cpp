/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginPreference.h"


PluginPreference::PluginPreference(int type, const wxString& name, const wxString& defaultValue, const wxString& title, const wxString& description, PluginPreferenceGroup* group, PluginPreferenceOptions options) {
  name_ = name;
  type_ = type;
  title_ = title;
  options_ = options;
  group_ = group;
  description_ = description;
  defaultValue_ = defaultValue;
  invalidated_ = false;
  hasBeenSet_ = false;
}


PluginPreferenceOptions PluginPreference::GetOptions() {
  return options_;
}


PluginPreferenceGroup* PluginPreference::GetGroup() {
  return group_;
}


wxString PluginPreference::GetName() {
  return name_;
}


wxString PluginPreference::GetValue() {
  if (hasBeenSet_) return value_;
  return GetDefaultValue();
}


int PluginPreference::GetType() {
  return type_;
}


wxString PluginPreference::GetTitle() {
  return title_;
}


wxString PluginPreference::GetDescription() {
  return description_;
}


void PluginPreference::SetValue(const wxString& value) {
  hasBeenSet_ = true;

  if (value_ == value) return;
  value_ = value;
  Invalidate();
}


wxString PluginPreference::GetDefaultValue() {
  return defaultValue_;
}


void PluginPreference::Invalidate() {
  invalidated_ = true;
}


void PluginPreference::Validate() {
  invalidated_ = false;
}


bool PluginPreference::IsInvalidated() {
  return invalidated_;
}


bool PluginPreference::ValueHasBeenSet() {
  return hasBeenSet_;
}