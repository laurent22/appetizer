/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include "QtGettext.h"


QtGettext* QtGettext::instance_ = NULL;


QtGettext* QtGettext::instance() {
  if (instance_) return instance_;
  instance_ = new QtGettext();
  return instance_;
}


void QtGettext::destroyInstance() {
  if (instance_) delete instance_;
  instance_ = NULL;
}


QtGettext::QtGettext() {
  gotAvailableLocales_ = false;
}


QtGettext::~QtGettext() {

}


QStringList QtGettext::availableLocales() const {
  if (gotAvailableLocales_) return availableLocales_;

  QDir dir(catalogueLocation());
  availableLocales_ = dir.entryList(QDir::Dirs | QDir::NoDotAndDotDot);
  gotAvailableLocales_ = true;

  return availableLocales_;
}


QString QtGettext::charset() const {
  const char* temp = moParser_.charset();
  if (!temp) return "";
  return QString::fromAscii(temp);
}


QString QtGettext::moFilePath() const {
  return QString::fromStdString(LauGettext::moFilePath());
}


QString QtGettext::getTranslation(const QString& originalString) const {
  GettextMessage* message = LauGettext::getTranslation(originalString.toAscii(), originalString.length());
  if (!message) return originalString;

  QString chars =charset();
  if (charset() == "utf-8") {
    return QString::fromUtf8(message->string);
  } else if (charset() == "utf-16") {
    const ushort* temp = (const ushort*)message->string;
    return QString::fromUtf16(temp, message->length);
  } else if (charset().indexOf("latin-1") >= 0) {
    return QString::fromLatin1(message->string);
  } else if (charset().indexOf("ucs-4") >= 0) {
    const uint* temp = (const uint*)message->string;
    return QString::fromUcs4(temp, message->length);
  }

  return QString::fromUtf8(message->string); 
}


void QtGettext::setCatalogueName(const QString& name) {
  LauGettext::setCatalogueName(name.toStdString());
}


void QtGettext::setCatalogueLocation(const QString& location) {
  LauGettext::setCatalogueLocation(location.toStdString());
}


void QtGettext::setLocale(const QString& localeCode) {
  LauGettext::setLocale(localeCode.toStdString());
}