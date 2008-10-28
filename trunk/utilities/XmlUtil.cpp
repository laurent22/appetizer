#include "XmlUtil.h"


void XmlUtil::AppendTextElement(TiXmlElement* targetElement, const char* elementName, const char* elementText) {
  TiXmlElement* element = new TiXmlElement(elementName);
  element->LinkEndChild(new TiXmlText(elementText));
  targetElement->LinkEndChild(element);
}


void XmlUtil::AppendTextElement(TiXmlElement* targetElement, const char* elementName, int elementText) {
  wxString s;
  s << elementText;
  XmlUtil::AppendTextElement(targetElement, elementName, s.mb_str());
}


wxString XmlUtil::ReadElementText(TiXmlHandle handle, const char* elementName) {
  TiXmlElement* element = handle.Child(elementName, 0).ToElement();
  if (element) return wxString(element->GetText(), wxConvUTF8);
  return wxEmptyString;
}