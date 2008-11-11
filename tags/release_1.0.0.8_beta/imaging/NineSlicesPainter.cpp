/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../precompiled.h"

#include "NineSlicesPainter.h"


NineSlicesPainter::NineSlicesPainter() {
  gridIsExplicitelySet_ = false;
  rotatedSourceBitmap_ = NULL;
  sourceBitmap_ = NULL;
  rotation_ = 0;
}


int NineSlicesPainter::GetRotation() {
  return rotation_;
}


void NineSlicesPainter::SetRotation(int rotation) {
  if (rotation == rotation_) return;
  rotation_ = rotation;
  wxDELETE(rotatedSourceBitmap_);
}


void NineSlicesPainter::SetGrid(int left, int top, int width, int height) {
  grid_.SetLeft(left);
  grid_.SetTop(top);
  grid_.SetWidth(width);
  grid_.SetHeight(height);
  gridIsExplicitelySet_ = true;
}


void NineSlicesPainter::LoadImage(const wxString& filePath, bool forceAlpha) {  
  filePath_ = filePath;
  wxDELETE(sourceBitmap_);
  sourceBitmap_ = new wxBitmap(filePath_, wxBITMAP_TYPE_PNG);  

  if (!sourceBitmap_->HasAlpha() && forceAlpha) {
    // @hack: the bitmap MUST have an alpha channel
    // See ImageButton::LoadImage
    wxImage tempImage(filePath_, wxBITMAP_TYPE_PNG);
    if (tempImage.GetWidth() > 0 && tempImage.GetHeight() > 0) {
      tempImage.InitAlpha();

      unsigned char pixelAlpha = tempImage.GetAlpha(0, 0);
      if (pixelAlpha == 0) {
        pixelAlpha = 1;
      } else if (pixelAlpha == 255) {
        pixelAlpha = 254;
      }
      
      wxDELETE(sourceBitmap_);
      sourceBitmap_ = new wxBitmap(tempImage);
    }
  }  
}


void NineSlicesPainter::Draw(wxDC *destDC, wxCoord x, wxCoord y, wxCoord width, wxCoord height) {

  if (width == 0 || height == 0) return;

  wxBitmap* workBitmap = NULL;

  if (rotation_ != 0) {
    if (!rotatedSourceBitmap_) {
      wxImage tempImage = sourceBitmap_->ConvertToImage();
      tempImage = tempImage.Rotate90(rotation_ == 90);
      rotatedSourceBitmap_ = new wxBitmap(tempImage);
    }

    workBitmap = rotatedSourceBitmap_;
  } else {
    workBitmap = sourceBitmap_;
  }

  if (width == workBitmap->GetWidth() && height == workBitmap->GetHeight()) {
    // Optimization: if the source bitmap has the same size as the
    // destination size, we blit it directly.
    wxMemoryDC sourceDC;
    sourceDC.SelectObject(*workBitmap);
    destDC->Blit(x, y, width, height, &sourceDC, 0, 0);
    sourceDC.SelectObject(wxNullBitmap);
    return;
  }

  int destX, destY, destWidth, destHeight;
  int sourceX, sourceY, sourceWidth, sourceHeight;
  
  wxRect grid;

  if (gridIsExplicitelySet_) {
    grid = wxRect(grid_.GetLeft(), grid_.GetTop(), grid_.GetWidth(), grid_.GetHeight());
  } else {
    grid.SetLeft(floor((double)(workBitmap->GetWidth() / 2)));
    grid.SetTop(floor((double)(workBitmap->GetHeight() / 2)));
    grid.SetWidth(1);
    grid.SetHeight(1);
    grid_ = grid;
  }

  if (rotation_ == 90) {
    int left = grid.GetLeft();
    int bottom = grid.GetBottom();
    int w = grid.GetWidth();
    int h = grid.GetHeight();
    grid.SetTop(left);
    grid.SetLeft(sourceBitmap_->GetHeight() - bottom - 1);
    grid.SetWidth(h);
    grid.SetHeight(w);
  } else if (rotation_ == -90) {
    int top = grid.GetTop();
    int right = grid.GetRight();
    int w = grid.GetWidth();
    int h = grid.GetHeight();
    grid.SetTop(sourceBitmap_->GetWidth() - right - 1);
    grid.SetLeft(top);
    grid.SetWidth(h);
    grid.SetHeight(w);
  }

  int rightWidth = workBitmap->GetWidth() - grid.GetRight();
  int bottomHeight = workBitmap->GetHeight() - grid.GetBottom();

  sourceDC_.SelectObject(*workBitmap);

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
        sourceWidth = workBitmap->GetWidth() - grid.GetRight();
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
        sourceHeight = workBitmap->GetHeight() - grid.GetBottom();
        break;

    } // switch

    Imaging::StretchBlit(destDC, &sourceDC_, destX, destY, destWidth, destHeight, sourceX, sourceY, sourceWidth, sourceHeight);

  } // for

  sourceDC_.SelectObject(wxNullBitmap);

}


NineSlicesPainter::~NineSlicesPainter() {
  wxDELETE(sourceBitmap_);
  wxDELETE(rotatedSourceBitmap_);
}