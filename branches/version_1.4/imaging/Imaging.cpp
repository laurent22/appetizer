/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

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


// The following two local functions are for the B-spline weighting of the
// bicubic sampling algorithm
static inline double spline_cube(double value)
{
    return value <= 0.0 ? 0.0 : value * value * value;
}


static inline double spline_weight(double value)
{
    return (spline_cube(value + 2) -
            4 * spline_cube(value + 1) +
            6 * spline_cube(value) -
            4 * spline_cube(value - 1)) / 6;
}



// This is the bicubic resampling algorithm
wxImage Imaging::ResampleBicubic(wxImage& image, int width, int height)
{
    // This function implements a Bicubic B-Spline algorithm for resampling.
    // This method is certainly a little slower than wxImage's default pixel
    // replication method, however for most reasonably sized images not being
    // upsampled too much on a fairly average CPU this difference is hardly
    // noticeable and the results are far more pleasing to look at.
    //
    // This particular bicubic algorithm does pixel weighting according to a
    // B-Spline that basically implements a Gaussian bell-like weighting
    // kernel. Because of this method the results may appear a bit blurry when
    // upsampling by large factors.  This is basically because a slight
    // gaussian blur is being performed to get the smooth look of the upsampled
    // image.

    // Edge pixels: 3-4 possible solutions
    // - (Wrap/tile) Wrap the image, take the color value from the opposite
    // side of the image.
    // - (Mirror)    Duplicate edge pixels, so that pixel at coordinate (2, n),
    // where n is nonpositive, will have the value of (2, 1).
    // - (Ignore)    Simply ignore the edge pixels and apply the kernel only to
    // pixels which do have all neighbours.
    // - (Clamp)     Choose the nearest pixel along the border. This takes the
    // border pixels and extends them out to infinity.
    //
    // NOTE: below the y_offset and x_offset variables are being set for edge
    // pixels using the "Mirror" method mentioned above

    wxImage ret_image;

    ret_image.Create(width, height, false);

    unsigned char* src_data = image.GetData();//M_IMGDATA->m_data;
      unsigned char* src_alpha = image.GetAlpha();
    unsigned char* dst_data = ret_image.GetData();
    unsigned char* dst_alpha = NULL;

    if ( src_alpha )
    {
        ret_image.SetAlpha();
        dst_alpha = ret_image.GetAlpha();
    }

    for ( int dsty = 0; dsty < height; dsty++ )
    {
        // We need to calculate the source pixel to interpolate from - Y-axis
      double srcpixy = double(dsty * image.GetHeight()) / height;
        double dy = srcpixy - (int)srcpixy;

        for ( int dstx = 0; dstx < width; dstx++ )
        {
            // X-axis of pixel to interpolate from
          double srcpixx = double(dstx * image.GetWidth()) / width;
            double dx = srcpixx - (int)srcpixx;

            // Sums for each color channel
            double sum_r = 0, sum_g = 0, sum_b = 0, sum_a = 0;

            // Here we actually determine the RGBA values for the destination pixel
            for ( int k = -1; k <= 2; k++ )
            {
                // Y offset
                int y_offset = srcpixy + k < 0.0
                                ? 0
                                : srcpixy + k >= image.GetHeight()
                                       ? image.GetHeight() - 1
                                       : (int)(srcpixy + k);

                // Loop across the X axis
                for ( int i = -1; i <= 2; i++ )
                {
                    // X offset
                    int x_offset = srcpixx + i < 0.0
                                    ? 0
                                    : srcpixx + i >= image.GetWidth()
                                            ? image.GetWidth() - 1
                                            : (int)(srcpixx + i);

                    // Calculate the exact position where the source data
                    // should be pulled from based on the x_offset and y_offset
                    int src_pixel_index = y_offset*image.GetWidth() + x_offset;

                    // Calculate the weight for the specified pixel according
                    // to the bicubic b-spline kernel we're using for
                    // interpolation
                    double
                        pixel_weight = spline_weight(i - dx)*spline_weight(k - dy);

                    // Create a sum of all velues for each color channel
                    // adjusted for the pixel's calculated weight
                    sum_r += src_data[src_pixel_index * 3 + 0] * pixel_weight;
                    sum_g += src_data[src_pixel_index * 3 + 1] * pixel_weight;
                    sum_b += src_data[src_pixel_index * 3 + 2] * pixel_weight;
                    if ( src_alpha )
                        sum_a += src_alpha[src_pixel_index] * pixel_weight;
                }
            }

            // Put the data into the destination image.  The summed values are
            // of double data type and are rounded here for accuracy
            dst_data[0] = (unsigned char)(sum_r + 0.5);
            dst_data[1] = (unsigned char)(sum_g + 0.5);
            dst_data[2] = (unsigned char)(sum_b + 0.5);
            dst_data += 3;

            if ( src_alpha )
                *dst_alpha++ = (unsigned char)sum_a;
        }
    }

    return ret_image;
}


wxIcon* Imaging::CreateIconFromPng(const wxString& filePath, int iconSize) {
  wxImage img(filePath, wxBITMAP_TYPE_PNG);
  if (!img.HasAlpha()) img.InitAlpha();
  img = Imaging::ResampleBicubic(img, iconSize, iconSize);
  
  wxIcon* output = new wxIcon();
  output->CopyFromBitmap(wxBitmap(img));
  
  return output;
}


void Imaging::StretchDrawBitmap(wxDC* destDC, wxBitmap& bitmap, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight) {
  if (destWidth <= 0 || destHeight <= 0) return;

  int srcWidth = bitmap.GetWidth();
  int srcHeight = bitmap.GetHeight();

  if (destWidth == srcWidth && destHeight == srcHeight) {
    destDC->DrawBitmap(bitmap, destX, destY);
    return;
  }

  double saveScaleX, saveScaleY;
  destDC->GetUserScale(&saveScaleX, &saveScaleY);

  double scaleX = (double)destWidth / (double)srcWidth;
  double scaleY = (double)destHeight / (double)srcHeight;

  destDC->SetUserScale(scaleX, scaleY);
  
  destDC->DrawBitmap(bitmap, destX, destY);

  destDC->SetUserScale(saveScaleX, saveScaleY);
}


void Imaging::StretchDrawIcon(wxDC* destDC, wxIcon& icon, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight) {
  if (destWidth <= 0 || destHeight <= 0) return;

  int srcWidth = icon.GetWidth();
  int srcHeight = icon.GetHeight();

  if (destWidth == srcWidth && destHeight == srcHeight) {
    destDC->DrawIcon(icon, destX, destY);
    return;
  }

  double saveScaleX, saveScaleY;
  destDC->GetUserScale(&saveScaleX, &saveScaleY);

  double scaleX = (double)destWidth / (double)srcWidth;
  double scaleY = (double)destHeight / (double)srcHeight;

  destDC->SetUserScale(scaleX, scaleY);
  
  destDC->DrawIcon(icon, destX, destY);

  destDC->SetUserScale(saveScaleX, saveScaleY);
}


bool Imaging::IsBadIcon(const wxIcon& icon) {
  return wxBitmap(icon).GetMask() != NULL;
}



//wxImage testConvertToImage(const wxBitmap& bitmap) {
//
//    wxImage image;
//
//    // create an wxImage object
//    int width = bitmap.GetWidth();
//    int height = bitmap.GetHeight();
//    image.Create( width, height );
//    unsigned char *data = image.GetData();
//    if( !data )
//    {
//        return wxNullImage;
//    }
//
//
//    HDC hScreenDC = ::GetDC(NULL);
//
//    HDC hMemDC = ::CreateCompatibleDC(hScreenDC);
//    ::ReleaseDC(NULL, hScreenDC);
//
//    HBITMAP hBitmap = (HBITMAP) bitmap.GetHBITMAP();
//
//    HBITMAP hOldBitmap = (HBITMAP)::SelectObject(hMemDC, hBitmap);
//
//    int i, j;
//    for (i = 0; i < bitmap.GetWidth(); i++)
//    {
//        for (j = 0; j < bitmap.GetHeight(); j++)
//        {
//            COLORREF color = ::GetPixel(hMemDC, i, j);
//            unsigned char red = GetRValue(color);
//            unsigned char green = GetGValue(color);
//            unsigned char blue = GetBValue(color);
//
//            image.SetRGB(i, j, red, green, blue);
//        }
//    }
//
//    ::SelectObject(hMemDC, hOldBitmap);
//    ::DeleteDC(hMemDC);
//
//    // Copy the palette from the source image
//    if (bitmap.GetPalette())
//        image.SetPalette(* bitmap.GetPalette());
//
//    return image;
//}
//
//
//
//

wxImage* Imaging::IconToImageWithAlpha(const wxIcon& icon) {
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

    wxImage* image = new wxImage(icon.GetWidth(), icon.GetHeight());
    image->InitAlpha();

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
            image->SetAlpha(x, y, 1);
            hackedPixelSet = true;
          } else {
            image->SetAlpha(x, y, 0);
          }
        } else {
          image->SetRGB(x, y, pixelColor.Red(), pixelColor.Green(), pixelColor.Blue());
        }
      }
    }

    if (!hackedPixelSet) {
      // Haven't set the hacked pixel transparency yet, probably because
      // all the pixels are opaque, so we do it:
      if (image->GetWidth() > 0 && image->GetHeight() > 0) image->SetAlpha(0, 0, 254);
    }    

    memDC.SelectObject(wxNullBitmap);

    return image;
  } else {
    //wxBitmap bmp(icon);


    //wxMemoryDC dc;
    //wxBitmap targetBitmap(bmp.GetWidth(), bmp.GetHeight());

    //dc.SelectObject(targetBitmap);
    //dc.SetBackgroundMode(wxTRANSPARENT);
    //dc.SetBrush(wxBrush(wxColour(255,255,255,wxALPHA_TRANSPARENT), wxTRANSPARENT));
    //dc.Clear();

    //dc.DrawBitmap(bmp, 0, 0);

    //for (int i = 0; i < bmp.GetWidth(); i++) {
    //  for (int j = 0; j < bmp.GetHeight(); j++) {
    //    wxColor c;
    //    dc.GetPixel(i, j, &c);

    //    wxLogDebug(_T("%d"), c.Alpha());
    //  }
    //}

    //dc.SelectObject(wxNullBitmap);

    //wxImage* image = new wxImage(bmp.ConvertToImage());

    wxImage* image = new wxImage(((wxBitmap)icon).ConvertToImage());
    
    

    return image;
  }
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


void Imaging::DrawColorOverlay(wxBitmap& bitmap, const wxColour& color) {
  int imageWidth = bitmap.GetWidth();
  int imageHeight = bitmap.GetHeight();

  wxBitmap bmp(imageWidth, imageHeight);
  wxMemoryDC dc;
  dc.SelectObject(bmp);
  dc.SetPen(wxPen(wxColour(0,0,0), 0, wxTRANSPARENT));
  dc.SetBrush(wxColour(color.Red(), color.Green(), color.Blue(), 255));
  dc.DrawRectangle(0, 0, imageWidth, imageHeight);
  dc.SelectObject(wxNullBitmap);

  wxImage image = bmp.ConvertToImage();
  if (!image.HasAlpha()) image.InitAlpha();

  wxImage sourceImage = bitmap.ConvertToImage();
  bool sourceImageHasAlpha = sourceImage.HasAlpha();
  int colorAlpha = color.Alpha();
  
  for (int i = 0; i < imageWidth; i++) {
    for (int j = 0; j < imageHeight; j++) {
      int newAlpha = colorAlpha;
      
      if (sourceImageHasAlpha) {        
        double p = ((double)(sourceImage.GetAlpha(i, j)) / 255.0);
        newAlpha *= p;
        if (newAlpha > 255) newAlpha = 255;
      }

      image.SetAlpha(i, j, newAlpha);
    }
  }

  bmp = wxBitmap(image);

  dc.SelectObject(bitmap);
  dc.DrawBitmap(bmp, 0, 0);
  dc.SelectObject(wxNullBitmap);
}


void Imaging::DrawLabelWithTransparency(wxDC* destination, const wxString& text, const wxRect& rect, int alignment, wxColor& backgroundColor) {

  wxMemoryDC dc;
  
  wxBitmap textBitmap(rect.GetWidth(), rect.GetHeight());

  dc.SelectObject(textBitmap);

  dc.SetFont(destination->GetFont());
  dc.SetTextForeground(destination->GetTextForeground());
  dc.SetBrush(wxBrush(wxColor(255,255,255)));
  dc.SetPen(wxPen(wxColor(255,0,0), 0));
  dc.DrawRectangle(0,0,rect.GetWidth(), rect.GetHeight());
  dc.DrawLabel(text, rect, alignment);  


  wxImage image(textBitmap.GetWidth(), textBitmap.GetHeight());
  image.InitAlpha();

  wxColour pixelColor;

  // @hack: To force the image to have an 8-bit alpha channel
  // we set one of the pixel alpha value to 1 or 254. Otherwise we'll get
  // a 1 bit alpha channel which still won't render properly.
  bool hackedPixelSet = false;
  wxColor textColor = destination->GetTextForeground();
  
  for (int x = 0; x < textBitmap.GetWidth(); x++) {
    for (int y = 0; y < textBitmap.GetHeight(); y++) {
      dc.GetPixel(x, y, &pixelColor); 

      //image.SetRGB(x, y, pixelColor.Red(), pixelColor.Green(), pixelColor.Blue());

      if (pixelColor.Red() != 255 && pixelColor.Green() != 255 && pixelColor.Blue() != 255) {
        //int alpha = floor(((float)pixelColor.Red() + (float)pixelColor.Green() + (float)pixelColor.Blue() / 3.0) * 255.0);
        int alpha = 255;
        image.SetAlpha(x, y, alpha);
        image.SetRGB(x, y, textColor.Red(), textColor.Green(), textColor.Blue());
      } else {
        if (!hackedPixelSet) {
          image.SetAlpha(x,y,1);
          hackedPixelSet = true;
        } else {
          image.SetAlpha(x, y, 0);
        }
        
      }

    }
  }

  if (!hackedPixelSet) {
    // Haven't set the hacked pixel transparency yet, probably because
    // all the pixels are opaque, so we do it:
    if (image.GetWidth() > 0 && image.GetHeight() > 0) image.SetAlpha(0, 0, 254);
  }    

  destination->DrawBitmap(wxBitmap(image), 0,0);



  dc.SelectObject(wxNullBitmap);



































  //wxMemoryDC dc;
  //
  //wxBitmap textBitmap(rect.GetWidth(), rect.GetHeight());

  //dc.SelectObject(textBitmap);

  //dc.SetFont(destination->GetFont());
  //dc.SetTextForeground(wxColor(0,0,0));
  //dc.SetBrush(wxBrush(backgroundColor));
  //dc.SetPen(wxPen(wxColor(255,0,0), 0));
  //dc.DrawRectangle(0,0,rect.GetWidth(), rect.GetHeight());
  //dc.DrawLabel(text, rect, alignment);  


  //wxImage image(textBitmap.GetWidth(), textBitmap.GetHeight());
  //image.InitAlpha();

  //wxColour pixelColor;

  //// @hack: To force the image to have an 8-bit alpha channel
  //// we set one of the pixel alpha value to 1 or 254. Otherwise we'll get
  //// a 1 bit alpha channel which still won't render properly.
  //bool hackedPixelSet = false;
  //wxColor textColor = destination->GetTextForeground();
  //
  //for (int x = 0; x < textBitmap.GetWidth(); x++) {
  //  for (int y = 0; y < textBitmap.GetHeight(); y++) {
  //    dc.GetPixel(x, y, &pixelColor); 

  //    image.SetRGB(x, y, pixelColor.Red(), pixelColor.Green(), pixelColor.Blue());

  //    //if (pixelColor.Red() != 255 && pixelColor.Green() != 255 && pixelColor.Blue() != 255) {
  //    //  //int alpha = floor(((float)pixelColor.Red() + (float)pixelColor.Green() + (float)pixelColor.Blue() / 3.0) * 255.0);
  //    //  int alpha = 255;
  //    //  image.SetAlpha(x, y, alpha);
  //    //  image.SetRGB(x, y, textColor.Red(), textColor.Green(), textColor.Blue());
  //    //} else {
  //    //  if (!hackedPixelSet) {
  //    //    image.SetAlpha(x,y,1);
  //    //    hackedPixelSet = true;
  //    //  } else {
  //    //    image.SetAlpha(x, y, 0);
  //    //  }
  //    //  
  //    //}

  //  }
  //}

  //if (!hackedPixelSet) {
  //  // Haven't set the hacked pixel transparency yet, probably because
  //  // all the pixels are opaque, so we do it:
  //  if (image.GetWidth() > 0 && image.GetHeight() > 0) image.SetAlpha(0, 0, 254);
  //}    

  //destination->DrawBitmap(wxBitmap(image), 0,0);



  //dc.SelectObject(wxNullBitmap);
}