#include "NineSlicesPanel.h"



NineSlicesPanel::NineSlicesPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  nineSlicesPainter_ = NULL;
}


void NineSlicesPanel::LoadImage(const wxString& filePath) {
  if (filePath_ == filePath) return;

  filePath_ = filePath;

  wxDELETE(nineSlicesPainter_);

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.

  Refresh();
}


void NineSlicesPanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if ((nineSlicesPainter_ == NULL) && (filePath_ != wxT(""))) {
    nineSlicesPainter_ = new NineSlicesPainter();
    nineSlicesPainter_->LoadImage(filePath_);
  }

  if (nineSlicesPainter_ != NULL) {

    wxMemoryDC destDC;
    destDC.SelectObject(*controlBitmap_);    
    nineSlicesPainter_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    destDC.SelectObject(wxNullBitmap);
  }
}