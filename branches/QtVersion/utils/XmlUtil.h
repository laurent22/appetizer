/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef AZXMLUTIL_H
#define AZXMLUTIL_H

namespace appetizer {

class XmlUtil {

public:
  
  static void appendTextElement(TiXmlElement* targetElement, const QString& elementName, const QString& elementText); 
  static void appendTextElement(TiXmlElement* targetElement, const QString& elementName, bool elementText); 
  static void appendTextElement(TiXmlElement* targetElement, const QString& elementName, int elementText);
  
  static QString readElementText(TiXmlHandle handle, const QString& elementName, const QString& defaultValue = "");
  static bool readElementTextAsBool(TiXmlHandle handle, const QString& elementName, bool defaultValue = false);
  static int readElementTextAsInt(TiXmlHandle handle, const QString& elementName, int defaultValue = 0);
  static bool readElementTextAsRect(TiXmlHandle handle, const QString& elementName, QRect& result);
  static bool readElementTextAsPoint(TiXmlHandle handle, const QString& elementName, QPoint& result);
  static QColor readElementTextAsColor(TiXmlHandle handle, const QString& elementName, QColor& defaultColor);
  static bool readElementTextAsStringList(TiXmlHandle handle, const QString& elementName, QStringList& result);

};

}
#endif // AZXMLUTIL_H
