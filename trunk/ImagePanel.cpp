#include "ImagePanel.h"
#include "Imaging.h"


ImagePanel::ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

}


void ImagePanel::LoadImage(const wxString& filePath) {
  if (filePath_ == filePath) return;

  filePath_ = filePath;

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.
  bitmap_ = wxBitmap(filePath, wxBITMAP_TYPE_PNG);

  Refresh();
}


void ImagePanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (bitmap_.GetWidth() == GetRect().GetWidth() && bitmap_.GetHeight() == GetRect().GetHeight()) {
    destDC.DrawBitmap(bitmap_, 0, 0);
  } else {    
    wxMemoryDC sourceDC;    
    sourceDC.SelectObject(bitmap_);    
    Imaging::StretchBlit(&destDC, &sourceDC, 0, 0, GetRect().GetWidth(), GetRect().GetHeight(), 0, 0, bitmap_.GetWidth(), bitmap_.GetHeight());
    sourceDC.SelectObject(wxNullBitmap);    
  }

  destDC.SelectObject(wxNullBitmap);
}


void ImagePanel::FitToContent() {
  SetSize(bitmap_.GetWidth(), bitmap_.GetHeight());
}