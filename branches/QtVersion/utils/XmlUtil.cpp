/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <XmlUtil.h>
using namespace appetizer;


void XmlUtil::appendTextElement(TiXmlElement* targetElement, const QString& elementName, const QString& elementText) {
  TiXmlElement* element = new TiXmlElement(elementName.toUtf8());
  element->LinkEndChild(new TiXmlText(elementText.toUtf8()));
  targetElement->LinkEndChild(element);
}


void XmlUtil::appendTextElement(TiXmlElement* targetElement, const QString& elementName, bool elementText) {
  QString s = elementText ? "1" : "0";
  XmlUtil::appendTextElement(targetElement, elementName, s);
}


void XmlUtil::appendTextElement(TiXmlElement* targetElement, const QString& elementName, int elementText) {
  QString s;
  s.setNum(elementText);
  XmlUtil::appendTextElement(targetElement, elementName, s);
}


QString XmlUtil::readElementText(TiXmlHandle handle, const QString& elementName, const QString& defaultValue) {
  TiXmlElement* element = handle.Child(elementName.toUtf8(), 0).ToElement();
  if (element) {
    const char* cString = element->GetText();
    if (!cString) return defaultValue;
    QString output = QString::fromUtf8(cString);
   
    return output;
  }
  return defaultValue;
}


bool XmlUtil::readElementTextAsBool(TiXmlHandle handle, const QString& elementName, bool defaultValue) {
  QString s = readElementText(handle, elementName, defaultValue ? "1" : "0");
  return s == "1" || s.toLower() == "true";
}


int XmlUtil::readElementTextAsInt(TiXmlHandle handle, const QString& elementName, int defaultValue) {
  QString s = readElementText(handle, elementName);
  bool ok;
  int output = s.toInt(&ok);
  if (!ok) return defaultValue;
  return output;
}



bool XmlUtil::readElementTextAsRect(TiXmlHandle handle, const QString& elementName, QRect& result) {
  QString s = readElementText(handle, elementName);
  QStringList list = s.split(",");
  if (list.size() < 4) return false;

  bool ok;
  int x = list[0].toInt(&ok); if (!ok) return false;
  int y = list[1].toInt(&ok); if (!ok) return false;
  int w = list[2].toInt(&ok); if (!ok) return false;
  int h = list[3].toInt(&ok); if (!ok) return false;

  result = QRect(x, y, w, h);
  return true;
}



bool XmlUtil::readElementTextAsPoint(TiXmlHandle handle, const QString& elementName, QPoint& result) {
  QString s = readElementText(handle, elementName);
  QStringList list = s.split(",");
  if (list.size() < 2) return false;

  bool ok;
  int x = list[0].toInt(&ok); if (!ok) return false;
  int y = list[1].toInt(&ok); if (!ok) return false;

  result = QPoint(x, y);
  return true;
}



bool XmlUtil::readElementTextAsColor(TiXmlHandle handle, const QString& elementName, QColor& result) {
  QString s = readElementText(handle, elementName);
  QStringList list = s.split(",");
  if (list.size() < 3) return false;

  bool ok;
  int r = list[0].toInt(&ok); if (!ok) return false;
  int g = list[1].toInt(&ok); if (!ok) return false;
  int b = list[2].toInt(&ok); if (!ok) return false;
  int a = 255;
  if (list.size() >= 4) {
    a = list[3].toInt(&ok);
    if (!ok) return false;
  }

  result = QColor(r, g, b, a);
  return true;
}


bool XmlUtil::readElementTextAsStringList(TiXmlHandle handle, const QString& elementName, QStringList& result) {
  QString s = readElementText(handle, elementName);
  result = s.split(",");
  return true;
}