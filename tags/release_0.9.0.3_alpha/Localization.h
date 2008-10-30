#ifndef __Localization_H
#define __Localization_H

#include <wx/wx.h>
#include <wx/textfile.h>
#include <wx/arrstr.h>
#include <vector>


struct LocalizationString {
  wxString Id;
  wxString Text;
};


class LocalizationLocale {

public:

  LocalizationLocale(const wxString& code);
  ~LocalizationLocale();
  wxString GetString(const wxString& stringId);
  wxString GetCode();
  void AddString(const wxString& stringId, const wxString& stringText);

private:

  std::vector<LocalizationString*> strings_;
  wxString code_;

};


class Localization {

public:

  Localization();
  ~Localization();
  void LoadLocale(const wxString& localeCode, const wxString& localeFolderPath);
  bool LocaleLoaded(const wxString& localeCode);
  static wxString GetLanguageName(const wxString& localeFilePath);
  void SetCurrentLocale(const wxString& localeCode);
  wxString GetString(const wxString& stringId, wxArrayString* parameters = NULL);
  static Localization* Instance;

  /**
   * Shortcut method which automatically creates the wxArrayString
   * @param stringId The string identifier
   * @param param1 Parameter that should replace %0%
   * @return The string
   */
  wxString GetString(const wxString& stringId, const wxString& param1);
  wxString GetString(const wxString& stringId, const wxString& param1, const wxString& param2);
  wxString GetString(const wxString& stringId, const wxString& param1, const wxString& param2, const wxString& param3);

  static void Initialize();

private:

  wxString currentLocaleCode_;
  std::vector<LocalizationLocale*> loadedLocales_;

};

// NOTE: Localization::Initialize() must have 
// been called for these macros to work
#define LOC(stringId) Localization::Instance->GetString(stringId)
#define LOC1(stringId, param1) Localization::Instance->GetString(stringId, param1)
#define LOC2(stringId, param1, param2) Localization::Instance->GetString(stringId, param1, param2)
#define LOC3(stringId, param1, param2, param3) Localization::Instance->GetString(stringId, param1, param2, param3)

#endif // __Localization_H