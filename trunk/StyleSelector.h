#ifndef __StyleSelector_H
#define __StyleSelector_H

#include "wx/wx.h" 
#include <list>
using namespace std;


struct StyleElement {
  wxString Name;
  wxString Value;
};


class StyleSelector {

  private:

    list<StyleElement> list_;

  public:

    StyleSelector();
    wxString GetStyle(wxString name);
    int GetStyleInt(wxString name);
    void SetStyle(wxString name, wxString value);
    void SetStyleInt(wxString name, int value);

};

#endif