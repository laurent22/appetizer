/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include "LauGettext.h"
#include <fstream>


#ifdef _WINDOWS
#define PATH_SEPARATOR "\\"
#else
#define PATH_SEPARATOR "/"
#endif



LauGettext* LauGettext::instance_ = NULL;


LauGettext* LauGettext::instance() {
  if (instance_) return instance_;
  instance_ = new LauGettext();
  return instance_;
}


void LauGettext::destroyInstance() {
  if (instance_) delete instance_;
  instance_ = NULL;
}


LauGettext::LauGettext() {
  catalogueName_ = "locale";
}


LauGettext::~LauGettext() {

}


std::string LauGettext::moFilePath() const {
  std::string filePath = catalogueLocation() + PATH_SEPARATOR + locale() + PATH_SEPARATOR + catalogueName() + ".mo";

  std::ifstream fileTest(filePath.c_str(), std::ios::in | std::ios::binary);
  if (fileTest) return filePath;

  filePath = catalogueLocation() + PATH_SEPARATOR + languageCode() + PATH_SEPARATOR + catalogueName() + ".mo";
  std::ifstream fileTest2(filePath.c_str(), std::ios::in | std::ios::binary);
  if (fileTest2) return filePath;

  return "";
}


GettextMessage* LauGettext::getTranslation(const char* originalString, int originalLength) {
  if (!moParser_.ready()) {
    std::string p = moFilePath();
    if (p == "") return NULL; // File doesn't exist or cannot be opened
    bool success = moParser_.parseFile(moFilePath().c_str());
    if (!success) {
      // The file could not be parsed
      return NULL;
    }
  }

  return moParser_.getTranslation(originalString, originalLength);
}


void LauGettext::setCatalogueName(std::string name) {
  catalogueName_ = name;
}


void LauGettext::setCatalogueLocation(std::string location) {
  catalogueLocation_ = location;
}


void LauGettext::setLocale(std::string localeCode) {
  size_t splitIndex = localeCode.find("_");
  if (splitIndex != localeCode.npos) {
    languageCode_ = localeCode.substr(0, splitIndex);
    countryCode_ = localeCode.substr(splitIndex + 1, 10);
  } else {
    languageCode_ = localeCode;
    countryCode_ = "";
  }

  locale_ = countryCode_ == "" ? languageCode_ : languageCode_ + "_" + countryCode_;

  moParser_.clearData();
}


#undef PATH_SEPARATOR