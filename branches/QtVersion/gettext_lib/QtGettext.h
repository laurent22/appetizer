/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Gettext_QtGettext_H
#define Gettext_QtGettext_H


#include "LauGettext.h"


#undef _
#define _(x) QtGettext::instance()->getTranslation(x)


class QtGettext : public LauGettext {

public:

  static QtGettext* instance();
  static void destroyInstance();

  QtGettext();
  ~QtGettext();
  void setCatalogueLocation(const QString& location);
  void setCatalogueName(const QString& name);
  void setLocale(const QString& localeCode);
  QString moFilePath() const;
  inline QString locale() const { return QString::fromStdString(LauGettext::locale()); }
  inline QString languageCode() const { return QString::fromStdString(LauGettext::languageCode()); }
  inline QString countryCode() const { return QString::fromStdString(LauGettext::countryCode()); }
  inline QString catalogueName() const { return QString::fromStdString(LauGettext::catalogueName()); }
  inline QString catalogueLocation() const { return QString::fromStdString(LauGettext::catalogueLocation()); }

  QString getTranslation(const QString& originalString) const; 
  QStringList availableLocales() const;

  QString charset() const;

private:

  static QtGettext* instance_;
  mutable bool gotAvailableLocales_;
  mutable QStringList availableLocales_;

};

#endif // Gettext_QtGettext_H
