/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Gettext_LauGettext_H
#define Gettext_LauGettext_H


#include "MoParser.h"


class LauGettext {

public:

  static LauGettext* instance();
  static void destroyInstance();

  LauGettext();
  ~LauGettext();
  void setCatalogueLocation(std::string location);
  void setCatalogueName(std::string name);
  void setLocale(std::string localeCode);
  std::string moFilePath() const;
  inline std::string locale() const { return locale_; }
  inline std::string languageCode() const { return languageCode_; }
  inline std::string countryCode() const { return countryCode_; }
  inline std::string catalogueName() const { return catalogueName_; }
  inline std::string catalogueLocation() const { return catalogueLocation_; }

  GettextMessage* getTranslation(const char* originalString, int originalLength); 

private:

  std::string catalogueLocation_;
  std::string languageCode_;
  std::string countryCode_;
  std::string locale_;
  std::string catalogueName_;
  GettextMoParser moParser_;

  static LauGettext* instance_;

};

#endif // Gettext_LauGettext_H
