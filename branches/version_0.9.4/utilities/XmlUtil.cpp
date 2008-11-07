/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "XmlUtil.h"
#include "StringUtil.h"
#include <wx/arrstr.h>


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


bool XmlUtil::ReadElementTextAsRect(TiXmlHandle handle, const char* elementName, wxRect& resultRect, wxRect defaultValue) {
  wxString s = XmlUtil::ReadElementText(handle, elementName);

  wxArrayString splitted;
  StringUtil::Split(s, splitted, _T(","));
  if (splitted.Count() < 4) return false;

  long x; if (!splitted[0].ToLong(&x)) return false;
  long y; if (!splitted[1].ToLong(&y)) return false;
  long w; if (!splitted[2].ToLong(&w)) return false;
  long h; if (!splitted[3].ToLong(&h)) return false;

  resultRect = wxRect(x, y, w, h);
  return true;
}


bool XmlUtil::ReadElementTextAsPoint(TiXmlHandle handle, const char* elementName, wxPoint& resultPoint, wxPoint defaultValue) {
  wxString s = XmlUtil::ReadElementText(handle, elementName);

  wxArrayString splitted;
  StringUtil::Split(s, splitted, _T(","));
  if (splitted.Count() < 2) return false;

  long x; if (!splitted[0].ToLong(&x)) return false;
  long y; if (!splitted[1].ToLong(&y)) return false;

  resultPoint = wxPoint(x, y);
  return true;
}

