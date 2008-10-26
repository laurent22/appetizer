#ifndef __OptionPanel_H
#define __OptionPanel_H

#include "wx/wx.h" 
#include "bitmap_controls/NineSlicesPanel.h"
#include <vector>
#include "OptionButton.h"


typedef std::vector<OptionButton*> OptionButtonVector;


class OptionPanel: public NineSlicesPanel {

public:

  OptionPanel(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
  void InvalidateLayout();
  void UpdateLayout();

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);

private:

  OptionButtonVector buttons_;
  bool layoutInvalidated_;

};

#endif // __OptionPanel_H