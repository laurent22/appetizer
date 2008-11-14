/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __Imaging_H
#define __Imaging_H


class Imaging {

public:
  
  static void StretchBlit(wxDC* destDC, wxDC* sourceDC, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight, wxCoord srcX, wxCoord srcY, wxCoord srcWidth, wxCoord srcHeight);
  static void DrawIconWithTransparency(wxDC* destination, const wxIcon& icon, int destX, int destY);
  static wxBitmap* IconToBitmapWithAlpha(const wxIcon& icon);
  static void ColorizeImage(wxImage& image, const wxColour& color);
  static void ColorizeImage(wxImage& image, unsigned char red, unsigned char green, unsigned char blue);

};

#endif