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

}


QtGettext::~QtGettext() {

}


QString QtGettext::moFilePath() const {
  return QString::fromStdString(LauGettext::moFilePath());
}


QString QtGettext::getTranslation(const QString& originalString) const {
  GettextMessage* message = LauGettext::getTranslation(originalString.toAscii(), originalString.length());
  if (!message) return "";

  // TODO: Handle charset
  const QString output = QString::fromUtf8(message->string);
  return output;
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