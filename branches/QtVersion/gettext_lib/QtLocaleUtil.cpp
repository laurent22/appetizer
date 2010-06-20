/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include "QtLocaleUtil.h"


QtLocaleUtil::QtLocaleUtil() {
  codeToLanguageE_["aa"] = "Afar";
  codeToLanguageE_["ab"] = "Abkhazian";
  codeToLanguageE_["af"] = "Afrikaans";
  codeToLanguageE_["am"] = "Amharic";
  codeToLanguageE_["an"] = "Aragonese";
  codeToLanguageE_["ar"] = "Arabic";
  codeToLanguageE_["as"] = "Assamese";
  codeToLanguageE_["ay"] = "Aymara";
  codeToLanguageE_["az"] = "Azerbaijani";
  codeToLanguageE_["ba"] = "Bashkir";
  codeToLanguageE_["be"] = "Byelorussian";
  codeToLanguageE_["bg"] = "Bulgarian";
  codeToLanguageE_["bh"] = "Bihari";
  codeToLanguageE_["bi"] = "Bislama";
  codeToLanguageE_["bn"] = "Bangla";
  codeToLanguageE_["bo"] = "Tibetan";
  codeToLanguageE_["br"] = "Breton";
  codeToLanguageE_["ca"] = "Catalan";
  codeToLanguageE_["co"] = "Corsican";
  codeToLanguageE_["cs"] = "Czech";
  codeToLanguageE_["cy"] = "Welsh";
  codeToLanguageE_["da"] = "Danish";
  codeToLanguageE_["de"] = "German";
  codeToLanguageE_["dz"] = "Bhutani";
  codeToLanguageE_["el"] = "Greek";
  codeToLanguageE_["en"] = "English";
  codeToLanguageE_["eo"] = "Esperanto";
  codeToLanguageE_["es"] = "Spanish";
  codeToLanguageE_["et"] = "Estonian";
  codeToLanguageE_["eu"] = "Basque";
  codeToLanguageE_["fa"] = "Persian";
  codeToLanguageE_["fi"] = "Finnish";
  codeToLanguageE_["fj"] = "Fiji";
  codeToLanguageE_["fo"] = "Faroese";
  codeToLanguageE_["fr"] = "French";
  codeToLanguageE_["fy"] = "Frisian";
  codeToLanguageE_["ga"] = "Irish";
  codeToLanguageE_["gd"] = "Gaelic";
  codeToLanguageE_["gl"] = "Galician";
  codeToLanguageE_["gn"] = "Guarani";
  codeToLanguageE_["gu"] = "Gujarati";
  codeToLanguageE_["ha"] = "Hausa";
  codeToLanguageE_["he"] = "Hebrew";
  codeToLanguageE_["hi"] = "Hindi";
  codeToLanguageE_["hr"] = "Croatian";
  codeToLanguageE_["hu"] = "Hungarian";
  codeToLanguageE_["hy"] = "Armenian";
  codeToLanguageE_["ia"] = "Interlingua";
  codeToLanguageE_["id"] = "Indonesian";
  codeToLanguageE_["ie"] = "Interlingue";
  codeToLanguageE_["ik"] = "Inupiak";
  codeToLanguageE_["is"] = "Icelandic";
  codeToLanguageE_["it"] = "Italian";
  codeToLanguageE_["iu"] = "Inuktitut";
  codeToLanguageE_["ja"] = "Japanese";
  codeToLanguageE_["jw"] = "Javanese";
  codeToLanguageE_["ka"] = "Georgian";
  codeToLanguageE_["kk"] = "Kazakh";
  codeToLanguageE_["kl"] = "Greenlandic";
  codeToLanguageE_["km"] = "Cambodian";
  codeToLanguageE_["kn"] = "Kannada";
  codeToLanguageE_["ko"] = "Korean";
  codeToLanguageE_["ks"] = "Kashmiri";
  codeToLanguageE_["ku"] = "Kurdish";
  codeToLanguageE_["ky"] = "Kirghiz";
  codeToLanguageE_["la"] = "Latin";
  codeToLanguageE_["ln"] = "Lingala";
  codeToLanguageE_["lo"] = "Laothian";
  codeToLanguageE_["lt"] = "Lithuanian";
  codeToLanguageE_["lv"] = "Lettish";
  codeToLanguageE_["mg"] = "Malagasy";
  codeToLanguageE_["mi"] = "Maori";
  codeToLanguageE_["mk"] = "Macedonian";
  codeToLanguageE_["ml"] = "Malayalam";
  codeToLanguageE_["mn"] = "Mongolian";
  codeToLanguageE_["mo"] = "Moldavian";
  codeToLanguageE_["mr"] = "Marathi";
  codeToLanguageE_["ms"] = "Malay";
  codeToLanguageE_["mt"] = "Maltese";
  codeToLanguageE_["my"] = "Burmese";
  codeToLanguageE_["na"] = "Nauru";
  codeToLanguageE_["ne"] = "Nepali";
  codeToLanguageE_["nl"] = "Dutch";
  codeToLanguageE_["no"] = "Norwegian";
  codeToLanguageE_["oc"] = "Occitan";
  codeToLanguageE_["om"] = "Oromo";
  codeToLanguageE_["or"] = "Oriya";
  codeToLanguageE_["pa"] = "Punjabi";
  codeToLanguageE_["pl"] = "Polish";
  codeToLanguageE_["ps"] = "Pushto";
  codeToLanguageE_["pt"] = "Portuguese";
  codeToLanguageE_["qu"] = "Quechua";
  codeToLanguageE_["rm"] = "Rhaeto-Romance";
  codeToLanguageE_["rn"] = "Kirundi";
  codeToLanguageE_["ro"] = "Romanian";
  codeToLanguageE_["ru"] = "Russian";
  codeToLanguageE_["rw"] = "Kinyarwanda";
  codeToLanguageE_["sa"] = "Sanskrit";
  codeToLanguageE_["sd"] = "Sindhi";
  codeToLanguageE_["sg"] = "Sangho";
  codeToLanguageE_["sh"] = "Serbo-Croatian";
  codeToLanguageE_["si"] = "Sinhalese";
  codeToLanguageE_["sk"] = "Slovak";
  codeToLanguageE_["sl"] = "Slovenian";
  codeToLanguageE_["sm"] = "Samoan";
  codeToLanguageE_["sn"] = "Shona";
  codeToLanguageE_["so"] = "Somali";
  codeToLanguageE_["sq"] = "Albanian";
  codeToLanguageE_["sr"] = "Serbian";
  codeToLanguageE_["ss"] = "Siswati";
  codeToLanguageE_["st"] = "Sesotho";
  codeToLanguageE_["su"] = "Sundanese";
  codeToLanguageE_["sv"] = "Swedish";
  codeToLanguageE_["sw"] = "Swahili";
  codeToLanguageE_["ta"] = "Tamil";
  codeToLanguageE_["te"] = "Telugu";
  codeToLanguageE_["tg"] = "Tajik";
  codeToLanguageE_["th"] = "Thai";
  codeToLanguageE_["ti"] = "Tigrinya";
  codeToLanguageE_["tk"] = "Turkmen";
  codeToLanguageE_["tl"] = "Tagalog";
  codeToLanguageE_["tn"] = "Setswana";
  codeToLanguageE_["to"] = "Tonga";
  codeToLanguageE_["tr"] = "Turkish";
  codeToLanguageE_["ts"] = "Tsonga";
  codeToLanguageE_["tt"] = "Tatar";
  codeToLanguageE_["tw"] = "Twi";
  codeToLanguageE_["ug"] = "Uighur";
  codeToLanguageE_["uk"] = "Ukrainian";
  codeToLanguageE_["ur"] = "Urdu";
  codeToLanguageE_["uz"] = "Uzbek";
  codeToLanguageE_["vi"] = "Vietnamese";
  codeToLanguageE_["vo"] = "Volapuk";
  codeToLanguageE_["wo"] = "Wolof";
  codeToLanguageE_["xh"] = "Xhosa";
  codeToLanguageE_["yi"] = "Yiddish";
  codeToLanguageE_["yo"] = "Yoruba";
  codeToLanguageE_["za"] = "Zhuang";
  codeToLanguageE_["zh"] = "Chinese";
  codeToLanguageE_["zu"] = "Zulu";

  codeToLanguage_["an"] = "Aragonés";
  codeToLanguage_["da"] = "Dansk";
  codeToLanguage_["de"] = "Deutsch";
  codeToLanguage_["en"] = "English";
  codeToLanguage_["es"] = "Español";
  codeToLanguage_["fr"] = "Français";
  codeToLanguage_["he"] = "עיברית";
  codeToLanguage_["it"] = "Italiano";
  codeToLanguage_["lt"] = "Lietuvių kalba";
  codeToLanguage_["nl"] = "Nederlands";
  codeToLanguage_["pl"] = "Polski";
  codeToLanguage_["pt"] = "Português";
  codeToLanguage_["ru"] = "Русский";
  codeToLanguage_["sk"] = "Slovenčina";
  codeToLanguage_["sq"] = "Shqip";
  codeToLanguage_["sr"] = "српски језик";
  codeToLanguage_["tr"] = "Türkçe";
  codeToLanguage_["ja"] = "日本語";  
  codeToLanguage_["ko"] = "한국말";
  codeToLanguage_["sv"] = "Svenska";    
  codeToLanguage_["el"] = "ελληνικά";    
  codeToLanguage_["zh"] = "中文";
  codeToLanguage_["ro"] = "Română"; 
  codeToLanguage_["et"] = "Eesti Keel"; 
  codeToLanguage_["vi"] = "Tiếng Việt"; 
  codeToLanguage_["hu"] = "Magyar"; 

  codeToCountry_["BR"] = "Brasil"; 
  codeToCountry_["CN"] = "中国"; 
}


QString QtLocaleUtil::getDisplayName(const QString& canonicalName) const {
  QString languageCode = getLanguageCodeOnly(canonicalName);
  QString countryCode = getCountryCodeOnly(canonicalName);

  QString output = getLanguageName(languageCode);

  QString extraString;

  if (countryCode != "") {
    if (languageCode == "zh" && countryCode == "CN") {
      extraString = "简体"; // "Simplified" in "Simplified Chinese"
    } else {
      extraString = getCountryName(countryCode);
    }    
  }

  if (languageCode == "zh" && (countryCode == "" || countryCode == "TW")) extraString = "繁體"; // "Traditional" in "Traditional Chinese"

  if (extraString != "") output += " (" + extraString + ")";

  return output;
}


QString QtLocaleUtil::getCountryName(const QString& countryCode) const {
  if (codeToCountry_.find(countryCode) == codeToCountry_.end()) return "";
  return codeToCountry_[countryCode];
}


QString QtLocaleUtil::getLanguageNameInEnglish(const QString& languageCode) const {
  if (codeToLanguageE_.find(languageCode) == codeToLanguageE_.end()) return "";
  return codeToLanguageE_[languageCode];
}


QString QtLocaleUtil::getLanguageName(const QString& languageCode, bool defaultToEnglish) const {
  if (codeToLanguage_.find(languageCode) == codeToLanguage_.end()) {
    if (defaultToEnglish) return getLanguageNameInEnglish(languageCode);
    return "";
  }
  return codeToLanguage_[languageCode];
}


QString QtLocaleUtil::getLanguageCodeOnly(const QString& canonicalName) const {
  if (canonicalName.length() < 2) return canonicalName;
  return canonicalName.mid(0, 2);
}


QString QtLocaleUtil::getCountryCodeOnly(const QString& canonicalName) const {
  if (canonicalName.length() <= 2) return "";
  return canonicalName.mid(3, 2);
}