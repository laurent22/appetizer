#ifndef __XmlUtil_H
#define __XmlUtil_H

#include <wx/wx.h>
#include "../third_party/tinyxml/tinyxml.h"


class XmlUtil {

public:

  static void AppendTextElement(TiXmlElement* targetElement, const char* elementName, const char* elementText); 
  static void AppendTextElement(TiXmlElement* targetElement, const char* elementName, int elementText);
};

#endif // __XmlUtil_H