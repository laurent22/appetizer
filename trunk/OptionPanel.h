#ifndef __OptionPanel_H
#define __OptionPanel_H

#include "wx/wx.h" 
#include "bitmap_controls/NineSlicesPanel.h"
#include <vector>
#include "OptionButton.h"


typedef std::vector<OptionButton*> OptionButtonVector;


enum {
  ID_BUTTON_OptionButton
};


class OptionPanel: public NineSlicesPanel {

public:

  OptionPanel(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
  void InvalidateLayout();
  void UpdateLayout();
  int GetRequiredWidth();

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  void OnImageButtonClick(wxCommandEvent& evt);

private:

  int requiredWidth_;
  OptionButtonVector buttons_;
  bool layoutInvalidated_;

  //DECLARE_EVENT_TABLE()

};

#endif // __OptionPanel_H