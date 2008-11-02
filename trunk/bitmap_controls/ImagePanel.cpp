/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "ImagePanel.h"
#include "../imaging/Imaging.h"


ImagePanel::ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

}


void ImagePanel::LoadImage(const wxString& filePath) {
  if (filePath_ == filePath) return;

  filePath_ = filePath;

  // Create a wxImage first and convert it to a wxBitmap
  // to make sure that the alpha channel is handled properly.
  // Also force the image to have an alpha channel so that
  // the blit operations on BitmapControl::controlBitmap_
  // don't fail. See BitmapControl::UpdateControlBitmap
  wxImage tempImage;
  tempImage.LoadFile(filePath, wxBITMAP_TYPE_PNG);
  if (!tempImage.HasAlpha()) tempImage.InitAlpha();
  bitmap_ = wxBitmap(tempImage);

  Refresh();
}


void ImagePanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();
  if (!controlBitmap_) return;

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