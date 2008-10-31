/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

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


void XmlUtil::AppendTextElement(TiXmlElement* targetElement, const char* elementName, bool elementText) {
  wxString s = elementText ? _T("true") : _T("false");
  XmlUtil::AppendTextElement(targetElement, elementName, s.mb_str());
}


void XmlUtil::AppendTextElement(TiXmlElement* targetElement, const char* elementName, wxString elementText) {
  XmlUtil::AppendTextElement(targetElement, elementName, elementText.mb_str());
}


wxString XmlUtil::ReadElementText(TiXmlHandle handle, const char* elementName, const wxString& defaultValue) {
  TiXmlElement* element = handle.Child(elementName, 0).ToElement();
  if (element) return wxString(element->GetText(), wxConvUTF8);
  return defaultValue;
}


bool XmlUtil::ReadElementTextAsBool(TiXmlHandle handle, const char* elementName, bool defaultValue) {
  wxString s = XmlUtil::ReadElementText(handle, elementName, defaultValue ? _T("1") : _T("0"));
  return s.Upper() == _T("TRUE") || s == _T("1");
}


int XmlUtil::ReadElementTextAsInt(TiXmlHandle handle, const char* elementName, int defaultValue) {
  wxString s = XmlUtil::ReadElementText(handle, elementName);
  long tempLong;
  if (!s.ToLong(&tempLong)) {
    return defaultValue;
  } else {
    return (int)tempLong;
  }
}