#include "StyleSelector.h"


StyleSelector::StyleSelector() {

}


wxString StyleSelector::GetStyle(wxString name) {
  list<StyleElement>::iterator iter;

  for (iter = pList.begin(); iter != pList.end(); iter++) {
    if (iter->Name == name) {
      return iter->Value;
    }
  }

  return "";
}


void StyleSelector::SetStyle(wxString name, wxString value) {
  list<StyleElement>::iterator iter;

  for (iter = pList.begin(); iter != pList.end(); iter++) {
    if (iter->Name == name) {
      iter->Value = value;
      return;
    }
  }

  StyleElement e = StyleElement();
  e.Name = name;
  e.Value = value;

  pList.push_back(e);
}


int StyleSelector::GetStyleInt(wxString name) {
  wxString s = GetStyle(name);
  if (s == "") return 0;
  long value;
  if(!s.ToLong(&value)) {
    // Could not convert
    return 0;
  }
  return (int)value;
}


void StyleSelector::SetStyleInt(wxString name, int value) {
  wxString s;
  s << value;
  SetStyle(name, s);
}