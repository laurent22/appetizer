/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "NineSlicesPanel.h"



NineSlicesPanel::NineSlicesPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  nineSlicesPainter_ = NULL;
  gridIsExplicitelySet_ = false;
}


NineSlicesPanel::~NineSlicesPanel() {
  wxDELETE(nineSlicesPainter_);
}


void NineSlicesPanel::SetGrid(int left, int top, int width, int height) {
  gridIsExplicitelySet_ = true;
  grid_.SetLeft(left);
  grid_.SetTop(top);
  grid_.SetWidth(width);
  grid_.SetHeight(height);
  InvalidateControlBitmap();
}


void NineSlicesPanel::SetGrid(const wxRect& rect) {
  SetGrid(rect.GetLeft(), rect.GetTop(), rect.GetWidth(), rect.GetHeight());
}


void NineSlicesPanel::LoadImage(const wxString& filePath) {
  if (filePath_ == filePath) return;

  filePath_ = filePath;

  wxDELETE(nineSlicesPainter_);

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.

  InvalidateControlBitmap();
  Refresh();
}


void NineSlicesPanel::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();
  if (!controlBitmap_) return;

  if ((nineSlicesPainter_ == NULL) && (filePath_ != wxEmptyString)) {
    nineSlicesPainter_ = new NineSlicesPainter();    
    nineSlicesPainter_->LoadImage(filePath_);
  }

  if (nineSlicesPainter_ != NULL) {
    if (gridIsExplicitelySet_) nineSlicesPainter_->SetGrid(grid_.GetLeft(), grid_.GetTop(), grid_.GetWidth(), grid_.GetHeight());
    nineSlicesPainter_->SetRotation(GetBitmapRotation());

    wxMemoryDC destDC;
    destDC.SelectObject(*controlBitmap_);
    nineSlicesPainter_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    destDC.SelectObject(wxNullBitmap);
  }
}