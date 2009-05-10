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
  secure_ = false;
  minValue_ = 0;
  maxValue_ = 100;
}


void PluginPreference::SetSecure(bool secure) {
  secure_ = secure;
}


bool PluginPreference::IsSecure() {
  return secure_;
}


PluginPreferenceOptions PluginPreference::GetOptions() {
  return options_;
}


void PluginPreference::SetRange(double minValue, double maxValue) {
  if (minValue > maxValue) return;

  minValue_ = minValue;
  maxValue_ = maxValue;
}


double PluginPreference::GetMinValue() { return minValue_; }
double PluginPreference::GetMaxValue() { return maxValue_; }


PluginPreferenceGroup* PluginPreference::GetGroup() {
  return group_;
}


wxString PluginPreference::GetName() {
  return name_;
}


bool PluginPreference::IsBoolean() { return GetType() == PluginPreferenceType::CheckBox; }
bool PluginPreference::IsInteger() { return GetType() == PluginPreferenceType::Spinner; }
bool PluginPreference::IsDouble() { return false; }
bool PluginPreference::IsString() { return !IsBoolean() && !IsInteger() && !IsDouble(); }



wxString PluginPreference::GetValue() {
  if (hasBeenSet_) return value_;
  return GetDefaultValue();
}


double PluginPreference::GetDoubleValue() {
  wxString value = GetValue();
  double output;
  bool done = value.ToDouble(&output);
  if (!done) return 0;
  return output;
}


int PluginPreference::GetIntValue() {
  wxString value = GetValue();
  long output;
  bool done = value.ToLong(&output);
  if (!done) return 0;
  return (int)output;
}


bool PluginPreference::GetBoolValue() {
  return value_ == _T("1") || value_.Lower() == _T("true");
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

  if (GetType() == PluginPreferenceType::Spinner) {

    double dValue;
    bool converted = value.ToDouble(&dValue);
    if (!converted) {
      ELOG(_T("PluginPreference: value is not a number"));
      return;
    }

    double fValue = (double)dValue;

    if (minValue_ != maxValue_) {
      if (fValue < minValue_ || fValue > maxValue_) {
        ELOG(wxString::Format(_T("PluginPreference: value of '%s' is not within correct range (%d, %d)."), GetName(), GetMinValue(), GetMaxValue()));
        return;
      }
    }

  }
    
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