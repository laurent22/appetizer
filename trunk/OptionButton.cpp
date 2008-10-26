#include "OptionButton.h"
#include "Controller.h"
#include "FilePaths.h"


extern ControllerSP gController;


OptionButton::OptionButton(wxWindow *owner, int id, wxPoint point, wxSize size):
ImageButton(owner, id, point, size) {
  LoadImage(FilePaths::SkinDirectory + _T("/OptionButton"));
  FitToImage();
}