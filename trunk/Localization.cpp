/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "Localization.h"
#include "FilePaths.h"


Localization* Localization::Instance = NULL;


LocalizationLocale::LocalizationLocale(const wxString& code) {
  code_ = code;
}


wxString LocalizationLocale::GetString(const wxString& stringId) {
  for (int i = 0; i < strings_.size(); i++) {
    LocalizationString* s = strings_.at(i);
    if (s->Id == stringId) return s->Text;
  }

  if (code_ == _T("en")) {
    return stringId;
  } else {
    return wxEmptyString;
  }
}


wxString LocalizationLocale::GetCode() {
  return code_;
}


void LocalizationLocale::AddString(const wxString& stringId, const wxString& stringText) {
  LocalizationString* s = new LocalizationString();
  s->Id = stringId;
  s->Text = stringText;
  strings_.push_back(s);
}


Localization::Localization() {
  currentLocaleCode_ = _T("en");
}


void Localization::SetCurrentLocale(const wxString& localeCode) {
  currentLocaleCode_ = localeCode;
}


bool Localization::LocaleLoaded(const wxString& localeCode) {
  for (int i = 0; i < loadedLocales_.size(); i++) {
    LocalizationLocale* locale = loadedLocales_.at(i);
    if (locale->GetCode() == localeCode) return true;
  }
  return false;
}


void Localization::LoadLocale(const wxString& localeCode, const wxString& localeFolderPath) {
  if (LocaleLoaded(localeCode)) return;

  wxTextFile textFile;
  textFile.Open(localeFolderPath + _T("/") + localeCode + _T(".txt"), wxConvUTF8);
  wxString line = textFile.GetFirstLine();

  LocalizationLocale* locale = new LocalizationLocale(localeCode);

  wxString currentString;
  wxString currentStringId;

  for (line = textFile.GetFirstLine(); !textFile.Eof(); line = textFile.GetNextLine()) {
    // Skip comments
    if (line[0] == _T('/') && line[1] == _T('/')) continue;

    if (line[0] == _T(' ')) {
      // Append to the current string
      currentString += _T("\n") + line.Mid(1, line.Len());
    } else {
      // Start a new string
      
      if (currentString != wxEmptyString) {
        locale->AddString(currentStringId, currentString);
        currentStringId = wxEmptyString;
        currentString = wxEmptyString;
      }

      int equalPos = line.Find(_T("="));
      if (equalPos == wxNOT_FOUND) continue; // Couldn't parse this line - no equal sign found
        
      currentStringId = line.Mid(0, equalPos);
      currentStringId.Trim();

      if (currentStringId == wxEmptyString) continue; // Couldn't parse this line - no identifier

      currentString = line.Mid(equalPos + 1, line.Len());
      currentString.Trim();
    }

  }

  textFile.Close();

  if (currentString != wxEmptyString) {
    locale->AddString(currentStringId, currentString);
  }

  loadedLocales_.push_back(locale);  
}


wxString Localization::GetLanguageName(const wxString& localeFilePath) {
  wxTextFile textFile;
  textFile.Open(localeFilePath, wxConvUTF8);
  wxString line = textFile.GetFirstLine();

  wxString result;

  int ssIndex = line.Find(_T("//"));
  if (ssIndex != wxNOT_FOUND) {
    result = line.Mid(ssIndex + 2, line.Len());
    result.Trim(false).Trim(true);
  }

  //if (line.Left(2) == _T("//")) {
  //  result = line.Mid(2, line.Len());
  //  result.Trim(false).Trim(true);
  //}

  textFile.Close();

  return result;
}


wxString Localization::GetString(const wxString& stringId, wxArrayString* parameters) {
  wxString result;
  LocalizationLocale* locale = NULL;

  for (int i = 0; i < loadedLocales_.size(); i++) {
    locale = loadedLocales_.at(i);
    if (locale->GetCode() != currentLocaleCode_) continue;
    result = locale->GetString(stringId);
  }

  // If we couldn't find the string in the current locale,
  // we default to english
  if (result == wxEmptyString && currentLocaleCode_ != _T("en")) {
    for (int i = 0; i < loadedLocales_.size(); i++) {
      locale = loadedLocales_.at(i);
      if (locale->GetCode() != _T("en")) continue;
      result = locale->GetString(stringId);
    }
  }

  if (parameters) {
    for (int i = 0; i < parameters->Count(); i++) {
      wxString s = (*parameters)[i];
      wxString tagNumber;
      tagNumber << i;
      wxString tag = _T("%") + tagNumber + _T("%");
      result.Replace(tag, s, true);
    }
  }

  return result;
}


wxString Localization::GetString(const wxString& stringId, const wxString& param1) {
  wxArrayString* a = new wxArrayString();
  a->Add(param1);
  wxString result = Localization::GetString(stringId, a);
  wxDELETE(a);
  return result;
}


wxString Localization::GetString(const wxString& stringId, const wxString& param1, const wxString& param2) {
  wxArrayString* a = new wxArrayString();
  a->Add(param1);
  a->Add(param2);
  wxString result = Localization::GetString(stringId, a);
  wxDELETE(a);
  return result;
}


wxString Localization::GetString(const wxString& stringId, const wxString& param1, const wxString& param2, const wxString& param3) {
  wxArrayString* a = new wxArrayString();
  a->Add(param1);
  a->Add(param2);
  a->Add(param3);
  wxString result = Localization::GetString(stringId, a);
  wxDELETE(a);
  return result;
}


void Localization::Initialize() {
  Localization::Instance = new Localization();
  Localization::Instance->LoadLocale(_T("en"), FilePaths::LocalesDirectory);
}


Localization::~Localization() {
  for (int i = 0; i < loadedLocales_.size(); i++) {
    wxDELETE(loadedLocales_.at(i))
  }
  loadedLocales_.clear();
}


LocalizationLocale::~LocalizationLocale() {
  for (int i = 0; i < strings_.size(); i++) {
    wxDELETE(strings_.at(i))
  }
  strings_.clear();
}