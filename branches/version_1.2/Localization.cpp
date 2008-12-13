/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Localization.h"


Localization* Localization::instance_ = NULL;


Localization::Localization() {
  languageCodeHashMap_[_T("aa")] = _T("Afar");
  languageCodeHashMap_[_T("ab")] = _T("Abkhazian");
  languageCodeHashMap_[_T("af")] = _T("Afrikaans");
  languageCodeHashMap_[_T("am")] = _T("Amharic");
  languageCodeHashMap_[_T("ar")] = _T("Arabic");
  languageCodeHashMap_[_T("as")] = _T("Assamese");
  languageCodeHashMap_[_T("ay")] = _T("Aymara");
  languageCodeHashMap_[_T("az")] = _T("Azerbaijani");
  languageCodeHashMap_[_T("ba")] = _T("Bashkir");
  languageCodeHashMap_[_T("be")] = _T("Byelorussian");
  languageCodeHashMap_[_T("bg")] = _T("Bulgarian");
  languageCodeHashMap_[_T("bh")] = _T("Bihari");
  languageCodeHashMap_[_T("bi")] = _T("Bislama");
  languageCodeHashMap_[_T("bn")] = _T("Bangla");
  languageCodeHashMap_[_T("bo")] = _T("Tibetan");
  languageCodeHashMap_[_T("br")] = _T("Breton");
  languageCodeHashMap_[_T("ca")] = _T("Catalan");
  languageCodeHashMap_[_T("co")] = _T("Corsican");
  languageCodeHashMap_[_T("cs")] = _T("Czech");
  languageCodeHashMap_[_T("cy")] = _T("Welsh");
  languageCodeHashMap_[_T("da")] = _T("Danish");
  languageCodeHashMap_[_T("de")] = _T("German");
  languageCodeHashMap_[_T("dz")] = _T("Bhutani");
  languageCodeHashMap_[_T("el")] = _T("Greek");
  languageCodeHashMap_[_T("en")] = _T("English");
  languageCodeHashMap_[_T("eo")] = _T("Esperanto");
  languageCodeHashMap_[_T("es")] = _T("Spanish");
  languageCodeHashMap_[_T("et")] = _T("Estonian");
  languageCodeHashMap_[_T("eu")] = _T("Basque");
  languageCodeHashMap_[_T("fa")] = _T("Persian");
  languageCodeHashMap_[_T("fi")] = _T("Finnish");
  languageCodeHashMap_[_T("fj")] = _T("Fiji");
  languageCodeHashMap_[_T("fo")] = _T("Faroese");
  languageCodeHashMap_[_T("fr")] = _T("French");
  languageCodeHashMap_[_T("fy")] = _T("Frisian");
  languageCodeHashMap_[_T("ga")] = _T("Irish");
  languageCodeHashMap_[_T("gd")] = _T("Gaelic");
  languageCodeHashMap_[_T("gl")] = _T("Galician");
  languageCodeHashMap_[_T("gn")] = _T("Guarani");
  languageCodeHashMap_[_T("gu")] = _T("Gujarati");
  languageCodeHashMap_[_T("ha")] = _T("Hausa");
  languageCodeHashMap_[_T("he")] = _T("Hebrew");
  languageCodeHashMap_[_T("hi")] = _T("Hindi");
  languageCodeHashMap_[_T("hr")] = _T("Croatian");
  languageCodeHashMap_[_T("hu")] = _T("Hungarian");
  languageCodeHashMap_[_T("hy")] = _T("Armenian");
  languageCodeHashMap_[_T("ia")] = _T("Interlingua");
  languageCodeHashMap_[_T("id")] = _T("Indonesian");
  languageCodeHashMap_[_T("ie")] = _T("Interlingue");
  languageCodeHashMap_[_T("ik")] = _T("Inupiak");
  languageCodeHashMap_[_T("is")] = _T("Icelandic");
  languageCodeHashMap_[_T("it")] = _T("Italian");
  languageCodeHashMap_[_T("iu")] = _T("Inuktitut");
  languageCodeHashMap_[_T("ja")] = _T("Japanese");
  languageCodeHashMap_[_T("jw")] = _T("Javanese");
  languageCodeHashMap_[_T("ka")] = _T("Georgian");
  languageCodeHashMap_[_T("kk")] = _T("Kazakh");
  languageCodeHashMap_[_T("kl")] = _T("Greenlandic");
  languageCodeHashMap_[_T("km")] = _T("Cambodian");
  languageCodeHashMap_[_T("kn")] = _T("Kannada");
  languageCodeHashMap_[_T("ko")] = _T("Korean");
  languageCodeHashMap_[_T("ks")] = _T("Kashmiri");
  languageCodeHashMap_[_T("ku")] = _T("Kurdish");
  languageCodeHashMap_[_T("ky")] = _T("Kirghiz");
  languageCodeHashMap_[_T("la")] = _T("Latin");
  languageCodeHashMap_[_T("ln")] = _T("Lingala");
  languageCodeHashMap_[_T("lo")] = _T("Laothian");
  languageCodeHashMap_[_T("lt")] = _T("Lithuanian");
  languageCodeHashMap_[_T("lv")] = _T("Lettish");
  languageCodeHashMap_[_T("mg")] = _T("Malagasy");
  languageCodeHashMap_[_T("mi")] = _T("Maori");
  languageCodeHashMap_[_T("mk")] = _T("Macedonian");
  languageCodeHashMap_[_T("ml")] = _T("Malayalam");
  languageCodeHashMap_[_T("mn")] = _T("Mongolian");
  languageCodeHashMap_[_T("mo")] = _T("Moldavian");
  languageCodeHashMap_[_T("mr")] = _T("Marathi");
  languageCodeHashMap_[_T("ms")] = _T("Malay");
  languageCodeHashMap_[_T("mt")] = _T("Maltese");
  languageCodeHashMap_[_T("my")] = _T("Burmese");
  languageCodeHashMap_[_T("na")] = _T("Nauru");
  languageCodeHashMap_[_T("ne")] = _T("Nepali");
  languageCodeHashMap_[_T("nl")] = _T("Dutch");
  languageCodeHashMap_[_T("no")] = _T("Norwegian");
  languageCodeHashMap_[_T("oc")] = _T("Occitan");
  languageCodeHashMap_[_T("om")] = _T("Oromo");
  languageCodeHashMap_[_T("or")] = _T("Oriya");
  languageCodeHashMap_[_T("pa")] = _T("Punjabi");
  languageCodeHashMap_[_T("pl")] = _T("Polish");
  languageCodeHashMap_[_T("ps")] = _T("Pushto");
  languageCodeHashMap_[_T("pt")] = _T("Portuguese");
  languageCodeHashMap_[_T("qu")] = _T("Quechua");
  languageCodeHashMap_[_T("rm")] = _T("Rhaeto-Romance");
  languageCodeHashMap_[_T("rn")] = _T("Kirundi");
  languageCodeHashMap_[_T("ro")] = _T("Romanian");
  languageCodeHashMap_[_T("ru")] = _T("Russian");
  languageCodeHashMap_[_T("rw")] = _T("Kinyarwanda");
  languageCodeHashMap_[_T("sa")] = _T("Sanskrit");
  languageCodeHashMap_[_T("sd")] = _T("Sindhi");
  languageCodeHashMap_[_T("sg")] = _T("Sangho");
  languageCodeHashMap_[_T("sh")] = _T("Serbo-Croatian");
  languageCodeHashMap_[_T("si")] = _T("Sinhalese");
  languageCodeHashMap_[_T("sk")] = _T("Slovak");
  languageCodeHashMap_[_T("sl")] = _T("Slovenian");
  languageCodeHashMap_[_T("sm")] = _T("Samoan");
  languageCodeHashMap_[_T("sn")] = _T("Shona");
  languageCodeHashMap_[_T("so")] = _T("Somali");
  languageCodeHashMap_[_T("sq")] = _T("Albanian");
  languageCodeHashMap_[_T("sr")] = _T("Serbian");
  languageCodeHashMap_[_T("ss")] = _T("Siswati");
  languageCodeHashMap_[_T("st")] = _T("Sesotho");
  languageCodeHashMap_[_T("su")] = _T("Sundanese");
  languageCodeHashMap_[_T("sv")] = _T("Swedish");
  languageCodeHashMap_[_T("sw")] = _T("Swahili");
  languageCodeHashMap_[_T("ta")] = _T("Tamil");
  languageCodeHashMap_[_T("te")] = _T("Telugu");
  languageCodeHashMap_[_T("tg")] = _T("Tajik");
  languageCodeHashMap_[_T("th")] = _T("Thai");
  languageCodeHashMap_[_T("ti")] = _T("Tigrinya");
  languageCodeHashMap_[_T("tk")] = _T("Turkmen");
  languageCodeHashMap_[_T("tl")] = _T("Tagalog");
  languageCodeHashMap_[_T("tn")] = _T("Setswana");
  languageCodeHashMap_[_T("to")] = _T("Tonga");
  languageCodeHashMap_[_T("tr")] = _T("Turkish");
  languageCodeHashMap_[_T("ts")] = _T("Tsonga");
  languageCodeHashMap_[_T("tt")] = _T("Tatar");
  languageCodeHashMap_[_T("tw")] = _T("Twi");
  languageCodeHashMap_[_T("ug")] = _T("Uighur");
  languageCodeHashMap_[_T("uk")] = _T("Ukrainian");
  languageCodeHashMap_[_T("ur")] = _T("Urdu");
  languageCodeHashMap_[_T("uz")] = _T("Uzbek");
  languageCodeHashMap_[_T("vi")] = _T("Vietnamese");
  languageCodeHashMap_[_T("vo")] = _T("Volapuk");
  languageCodeHashMap_[_T("wo")] = _T("Wolof");
  languageCodeHashMap_[_T("xh")] = _T("Xhosa");
  languageCodeHashMap_[_T("yi")] = _T("Yiddish");
  languageCodeHashMap_[_T("yo")] = _T("Yoruba");
  languageCodeHashMap_[_T("za")] = _T("Zhuang");
  languageCodeHashMap_[_T("zh")] = _T("Chinese");
  languageCodeHashMap_[_T("zu")] = _T("Zulu");

  languageCodeHashMapS_[_T("da")] = _T("Dansk");
  languageCodeHashMapS_[_T("de")] = _T("Deutsch");
  languageCodeHashMapS_[_T("en")] = _T("English");
  languageCodeHashMapS_[_T("es")] = _T("Español");
  languageCodeHashMapS_[_T("fr")] = _T("Français");
  languageCodeHashMapS_[_T("he")] = _T("עיברית");
  languageCodeHashMapS_[_T("it")] = _T("Italiano");
  languageCodeHashMapS_[_T("lt")] = _T("Lietuvių kalba");
  languageCodeHashMapS_[_T("nl")] = _T("Nederlands");
  languageCodeHashMapS_[_T("pl")] = _T("Polski");
  languageCodeHashMapS_[_T("pt")] = _T("Português");
  languageCodeHashMapS_[_T("ru")] = _T("Русский");
  languageCodeHashMapS_[_T("sk")] = _T("Slovenčina");
  languageCodeHashMapS_[_T("sq")] = _T("Shqip");
  languageCodeHashMapS_[_T("sr")] = _T("српски језик");
  languageCodeHashMapS_[_T("tr")] = _T("Türkçe");
  languageCodeHashMapS_[_T("ja")] = _T("日本語");  
}


wxString Localization::GetLanguageNameInEnglish(const wxString& languageCode) {
  return languageCodeHashMap_[languageCode];
}


wxString Localization::GetLanguageName(const wxString& languageCode, bool defaultToEnglish) {
  wxString output = languageCodeHashMapS_[languageCode];
  if (output != wxEmptyString) return output;
  return GetLanguageNameInEnglish(languageCode);
}


wxString Localization::GetLanguageCodeOnly(const wxString& canonicalName) {
  if (canonicalName.Len() < 2) return canonicalName;
  return canonicalName.Mid(0, 2);
}


Localization* Localization::Instance() {
  if (!Localization::instance_) Localization::instance_ = new Localization();
  return Localization::instance_;
}


Localization::~Localization() {
  
}


void Localization::Destroy() {
  wxDELETE(Localization::instance_);
}