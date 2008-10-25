#include "OptionPanel.h"
#include "Controller.h"


extern ControllerSP gController;


OptionPanel::OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(gController->GetFilePaths().SkinDirectory + _T("/OptionPanel.png"));
}