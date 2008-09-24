#include "ImagePanel.h"
#include "Imaging.h"


ImagePanel::ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

}


void ImagePanel::LoadImage(const wxString& filePath) {
  if (pFilePath == filePath) return;

  pFilePath = filePath;

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.
  pBitmap = wxBitmap(filePath, wxBITMAP_TYPE_PNG);

  Refresh();
}


void ImagePanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  wxMemoryDC destDC;
  destDC.SelectObject(*pControlBitmap);

  if (pBitmap.GetWidth() == GetRect().GetWidth() && pBitmap.GetHeight() == GetRect().GetHeight()) {
    destDC.DrawBitmap(pBitmap, 0, 0);
  } else {    
    wxMemoryDC sourceDC;    
    sourceDC.SelectObject(pBitmap);    
    Imaging::StretchBlit(&destDC, &sourceDC, 0, 0, GetRect().GetWidth(), GetRect().GetHeight(), 0, 0, pBitmap.GetWidth(), pBitmap.GetHeight());
    sourceDC.SelectObject(wxNullBitmap);    
  }

  destDC.SelectObject(wxNullBitmap);
}


void ImagePanel::FitToContent() {
  SetSize(pBitmap.GetWidth(), pBitmap.GetHeight());
}