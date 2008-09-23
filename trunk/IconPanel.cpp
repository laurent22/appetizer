#include "IconPanel.h"


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(wxT("Data/Skin/Default/BarInnerPanel.png"));
}