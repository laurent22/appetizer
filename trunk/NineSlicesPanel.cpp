#include "NineSlicesPanel.h"



NineSlicesPanel::NineSlicesPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  pNineSlicesPainter = NULL;
}


void NineSlicesPanel::LoadImage(const wxString& filePath) {
  if (pFilePath == filePath) return;

  pFilePath = filePath;

  wxDELETE(pNineSlicesPainter);

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.

  Refresh();
}


void NineSlicesPanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if ((pNineSlicesPainter == NULL) && (pFilePath != "")) {
    pNineSlicesPainter = new NineSlicesPainter();
    pNineSlicesPainter->LoadImage(pFilePath);
  }

  if (pNineSlicesPainter != NULL) {

    wxMemoryDC destDC;
    destDC.SelectObject(*pControlBitmap);    
    pNineSlicesPainter->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    destDC.SelectObject(wxNullBitmap);
  }
}