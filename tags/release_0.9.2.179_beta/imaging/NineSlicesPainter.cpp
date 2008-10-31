/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "NineSlicesPainter.h"


NineSlicesPainter::NineSlicesPainter() {
  gridIsExplicitelySet_ = false;
}


void NineSlicesPainter::SetGrid(int left, int top, int width, int height) {
  grid_.SetLeft(left);
  grid_.SetTop(top);
  grid_.SetWidth(width);
  grid_.SetHeight(height);
  gridIsExplicitelySet_ = true;
}


void NineSlicesPainter::LoadImage(const wxString& filePath, bool forceAlpha) {  
  sourceDC_.SelectObject(wxNullBitmap);

  filePath_ = filePath;
  sourceBitmap_ = wxBitmap(filePath_, wxBITMAP_TYPE_PNG);  

  if (!sourceBitmap_.HasAlpha() && forceAlpha) {
    // @hack: the bitmap MUST have an alpha channel
    // See ImageButton::LoadImage
    wxImage tempImage(filePath_, wxBITMAP_TYPE_PNG);
    if (tempImage.GetWidth() > 0 && tempImage.GetHeight() > 0) {
      tempImage.InitAlpha();
      tempImage.SetAlpha(0, 0, 254);
      sourceBitmap_ = wxBitmap(tempImage);
    }
  }  

  sourceDC_.SelectObject(sourceBitmap_);
}


void NineSlicesPainter::Draw(wxDC *destDC, wxCoord x, wxCoord y, wxCoord width, wxCoord height) {

  if (width == 0 || height == 0) return;

  if (width == sourceBitmap_.GetWidth() && height == sourceBitmap_.GetHeight()) {
    // Optimization: if the source bitmap has the same size as the
    // destination size, we blit it directly.
    wxMemoryDC sourceDC;
    sourceDC.SelectObject(sourceBitmap_);
    destDC->Blit(x, y, width, height, &sourceDC, 0, 0);
    sourceDC.SelectObject(wxNullBitmap);
    return;
  }

  int destX, destY, destWidth, destHeight;
  int sourceX, sourceY, sourceWidth, sourceHeight;
  
  wxRect grid;

  if (gridIsExplicitelySet_) {
    grid = grid_;
  } else {
    grid.SetLeft(floor((double)(sourceBitmap_.GetWidth() / 2)));
    grid.SetTop(floor((double)(sourceBitmap_.GetHeight() / 2)));
    grid.SetWidth(1);
    grid.SetHeight(1);
    grid_ = grid;
  }

  int rightWidth = sourceBitmap_.GetWidth() - grid.GetRight();
  int bottomHeight = sourceBitmap_.GetHeight() - grid.GetBottom();

  for (int i = 0; i < 9; i++) {

    switch(i) {

      case 0: case 3: case 6:

        destX = 0;
        destWidth = grid.GetLeft();
        sourceX = 0;
        sourceWidth = grid.GetLeft();
        break;

      case 1: case 4: case 7:

        destX = grid.GetLeft();
        destWidth = width - destX - rightWidth;
        sourceX = grid.GetLeft();
        sourceWidth = grid.GetWidth();
        break;

      case 2: case 5: case 8:

        destX = width - rightWidth;
        destWidth = rightWidth;
        sourceX = grid.GetRight();
        sourceWidth = sourceBitmap_.GetWidth() - grid.GetRight();
        break;

    } // switch

    switch(i) {

      case 0:

        destY = 0;
        destHeight = grid.GetTop();
        sourceY = 0;
        sourceHeight = grid.GetTop();
        break;

      case 3:

        destY = grid.GetTop();
        destHeight = height - destY - bottomHeight;
        sourceY = grid.GetTop();
        sourceHeight = grid.GetHeight();
        break;

      case 6:

        destY = height - bottomHeight;
        destHeight = bottomHeight;
        sourceY = grid.GetBottom();
        sourceHeight = sourceBitmap_.GetHeight() - grid.GetBottom();
        break;

    } // switch

    Imaging::StretchBlit(destDC, &sourceDC_, destX, destY, destWidth, destHeight, sourceX, sourceY, sourceWidth, sourceHeight);

  } // for

}