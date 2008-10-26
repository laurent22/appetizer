#include "OptionButton.h"
#include "Controller.h"


extern ControllerSP gController;


OptionButton::OptionButton(wxWindow *owner, int id, wxPoint point, wxSize size):
ImageButton(owner, id, point, size) {
  LoadImage(gController->GetFilePaths().SkinDirectory + _T("/OptionButton"));
  FitToImage();
}