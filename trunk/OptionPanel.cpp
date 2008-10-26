#include "OptionPanel.h"
#include "Controller.h"


extern ControllerSP gController;


OptionPanel::OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(gController->GetFilePaths().SkinDirectory + _T("/OptionPanel.png"));

  wxStringList buttonNames;
  buttonNames.Add(_T("close"));
  buttonNames.Add(_T("minimize"));
  buttonNames.Add(_T("eject"));

  for (int i = 0; i < buttonNames.size(); i++) {
    OptionButton* button = new OptionButton(this, wxID_ANY, wxPoint(0,0), wxDefaultSize);
    button->SetName(buttonNames[i]);
  }
}