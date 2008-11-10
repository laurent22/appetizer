/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "Imaging.h"


void Imaging::StretchBlit(wxDC* destDC, wxDC* sourceDC, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight, wxCoord srcX, wxCoord srcY, wxCoord srcWidth, wxCoord srcHeight) {
  if (destWidth <= 0 || destHeight <= 0 || srcWidth <= 0 || srcHeight <= 0) return;
  
  double saveScaleX, saveScaleY;
  sourceDC->GetUserScale(&saveScaleX, &saveScaleY);

  double scaleX = (double)srcWidth / (double)destWidth;
  double scaleY = (double)srcHeight / (double)destHeight;

  sourceDC->SetUserScale(scaleX, scaleY);
  
  destDC->Blit(destX, destY, destWidth, destHeight, sourceDC, srcX / scaleX, srcY / scaleY);

  sourceDC->SetUserScale(saveScaleX, saveScaleY);
}


wxBitmap* Imaging::IconToBitmapWithAlpha(const wxIcon& icon) {
  // If the icon has a mask, it means that it has
  // a 1-bit alpha channel, which means that it won't render properly
  // when drawn on a bitmap initialized with UseAlpha()

  if (wxBitmap(icon).GetMask()) {
    int maskTolerancy = 4;

    wxColour maskColor(255, 0, 255);
    wxPen pen;  

    // - Create an icon with the same size as the icon
    // - Fill the bitmap with the mask color
    // - and paint the icon on top of it.

    wxBitmap bitmap(icon.GetWidth(), icon.GetHeight(), 32);

    wxMemoryDC memDC;
    memDC.SelectObject(bitmap);
    wxBrush brush(maskColor);
    pen.SetColour(maskColor);
    memDC.SetBrush(brush);
    memDC.SetPen(pen);
    memDC.DrawRectangle(0, 0, icon.GetWidth(), icon.GetHeight());
    memDC.DrawIcon(icon, 0, 0);

    // Create an image with the same size as the icon and give it an
    // alpha channel. We are going to copy the above bitmap pixel by pixel.
    // If the color is the same as the mask color, we paint a transparent
    // pixel, otherwise we just paint the color.

    wxImage image(icon.GetWidth(), icon.GetHeight());
    image.InitAlpha();

    wxColour pixelColor;

    // @hack: To force the image to have an 8-bit alpha channel
    // we set one of the pixel alpha value to 1 or 254. Otherwise we'll get
    // a 1 bit alpha channel which still won't render properly.
    bool hackedPixelSet = false;
    
    for (int x = 0; x < bitmap.GetWidth(); x++) {
      for (int y = 0; y < bitmap.GetHeight(); y++) {
        memDC.GetPixel(x, y, &pixelColor); 

        // @yetanotherhack: It looks like some icons (like c:\windows\shell32.dll,0) have
        // a nearly invisible gray layer on top of them. It means that when they
        // are drawn on top of the mask color, it doesn't produce rgb(255, 0, 255) as it should
        // but rgb(251, 4, 251). Because of that, we need to allow for some tolerancy when looking
        // for the mask color. 4 seems to work well.
        // Note: on a second though, maybe it has something to do with the icon using a palette, and
        // the pink color being adjusted to fit within that palette? In theory, it shouldn't since the
        // bitmap we draw on is 32-bits but who knows...
        if (abs(pixelColor.Red() - maskColor.Red()) <= maskTolerancy &&
            abs(pixelColor.Green() - maskColor.Green()) <= maskTolerancy &&
            abs(pixelColor.Blue() - maskColor.Blue()) <= maskTolerancy) {
          if (!hackedPixelSet) {
            image.SetAlpha(x, y, 1);
            hackedPixelSet = true;
          } else {
            image.SetAlpha(x, y, 0);
          }
        } else {
          image.SetRGB(x, y, pixelColor.Red(), pixelColor.Green(), pixelColor.Blue());
        }
      }
    }

    if (!hackedPixelSet) {
      // Haven't set the hacked pixel transparency yet, probably because
      // all the pixels are opaque, so we do it:
      if (image.GetWidth() > 0 && image.GetHeight() > 0) image.SetAlpha(0, 0, 254);
    }    

    memDC.SelectObject(wxNullBitmap);

    return new wxBitmap(image);
  } else {
    return new wxBitmap(icon);
  }
}


void Imaging::DrawIconWithTransparency(wxDC* destination, const wxIcon& icon, int destX, int destY) {
  if (wxBitmap(icon).GetMask()) {
    wxBitmap* bitmap = Imaging::IconToBitmapWithAlpha(icon);
    destination->DrawBitmap(*bitmap, destX, destY);
    wxDELETE(bitmap);
  } else {
    destination->DrawIcon(icon, destX, destY);
  }
}


void Imaging::ColorizeImage(wxImage& image, unsigned char red, unsigned char green, unsigned char blue) {
  int imageWidth = image.GetWidth();
  int imageHeight = image.GetHeight();
  for (int i = 0; i < imageWidth; i++) {
    for (int j = 0; j < imageHeight; j++) {
      image.SetRGB(i, j, red, green, blue);
    }
  }
}


void Imaging::ColorizeImage(wxImage& image, const wxColour& color) {
  Imaging::ColorizeImage(image, color.Red(), color.Green(), color.Blue());
}