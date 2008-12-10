/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __Localization_H
#define __Localization_H


WX_DECLARE_STRING_HASH_MAP(wxString, LanguageCodeHashMap);
WX_DECLARE_STRING_HASH_MAP(wxString, LanguageCodeHashMapS);


class Localization {

public:

  Localization();
  ~Localization();
  static void Destroy();
  static Localization* Instance();
  wxString GetLanguageName(const wxString& languageCode, bool defaultToEnglish = true);
  wxString GetLanguageNameInEnglish(const wxString& languageCode);
  wxString GetLanguageCodeOnly(const wxString& canonicalName);

private:

  static Localization* instance_;
  LanguageCodeHashMap languageCodeHashMap_;
  LanguageCodeHashMapS languageCodeHashMapS_;
  
};

#endif // __Localization_H