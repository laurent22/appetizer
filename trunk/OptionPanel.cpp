#include "OptionPanel.h"
#include "Controller.h"
#include "FilePaths.h"


extern ControllerSP gController;


OptionPanel::OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(FilePaths::SkinDirectory + _T("/OptionPanel.png"));

  wxStringList buttonNames;
  buttonNames.Add(_T("close"));
  buttonNames.Add(_T("minimize"));
  buttonNames.Add(_T("eject"));

  for (int i = 0; i < buttonNames.size(); i++) {
    OptionButton* button = new OptionButton(this);
    button->SetName(buttonNames[i]);
  }
}


void OptionPanel::InvalidateLayout() {
  layoutInvalidated_ = true;
  Refresh();
}


void OptionPanel::OnPaint(wxPaintEvent& evt) {
  BitmapControl::OnPaint(evt);
  
  if (layoutInvalidated_) UpdateLayout();
}


void OptionPanel::OnSize(wxSizeEvent& evt) {
  BitmapControl::OnSize(evt);

  InvalidateLayout();
}


void OptionPanel::UpdateLayout() {
  layoutInvalidated_ = false;

}
