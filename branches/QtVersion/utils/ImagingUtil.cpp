/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <ImagingUtil.h>
using namespace appetizer;


QImage ImagingUtil::addBorderToImage(const QImage& image, const QColor& color, int left, int top, int right, int bottom) {  
  if (top < 0) top = left;
  if (right < 0) right = left;
  if (bottom < 0) bottom = left;
    
  if (top <= 0 && right <= 0 && top <= 0 && bottom <= 0) return image;

  QRgb borderColor = color.rgba();
  QImage output(image.width() + left + right, image.height() + top + bottom, image.format());
  int outputWidth = output.width();
  int outputHeight = output.height();
  int inputWidth = image.width();
  int inputHeight = image.height();

  for (int y = 0; y < outputHeight; y++) {
    QRgb* inLine = NULL;
    int inputY = y - top;
    if (inputY >= 0 && inputY < inputHeight) inLine = (QRgb*)image.scanLine(inputY);
    QRgb* outLine = (QRgb*)output.scanLine(y);
    for (int x = 0; x < outputWidth; x++) {
      int inputX = x - left;
      if (!inLine || inputX < 0 || inputX >= inputWidth) {
        outLine[x] = borderColor;
      } else {
        outLine[x] = inLine[inputX];
      }
    }
  }
  
  return output;
}


QImage ImagingUtil::fillKeepAlpha(const QImage& image, const QColor& color) {
  QImage output = image;
  int r = color.red();
  int g = color.green();
  int b = color.blue();
  int a = color.alpha();

  for (int y = 0; y < output.height(); y++) {
    QRgb* scanline = (QRgb*)output.scanLine(y);
    for (int x = 0; x < output.width(); x++) {
      QRgb c = scanline[x];
      int destAlpha = qAlpha(c);
      int newAlpha = (destAlpha * a) / 255;
      scanline[x] = qRgba(r, g, b, newAlpha);
    }
  }

  return output;
}


QImage ImagingUtil::horizontalBlur(const QImage& image, int radius) {
  if (radius <= 0) return image;
  
  QImage input = image;
  QImage output = input;

  int imageWidth = input.width();
  int imageHeight = input.height();
  double r = 0; double g = 0; double b = 0; double a = 0;
  int d = radius * 2 + 1;

  for (int y = 0; y < imageHeight; ++y) {
    QRgb* scanline = (QRgb*)input.scanLine(y);
    QRgb* outScanline = (QRgb*)output.scanLine(y);
    for (int x = 0; x < imageWidth; ++x) {
      r = 0; g = 0; b = 0; a = 0;
      for (int kx = -radius; kx <= radius; ++kx) {
        int scanlineX = x + kx;
        if (scanlineX < 0 || scanlineX >= imageWidth) continue;

        QRgb c = scanline[scanlineX];
        r += qRed(c); g += qGreen(c); b += qBlue(c); a += qAlpha(c);
      }
      outScanline[x] = qRgba(r/d, g/d, b/d, a/d);
    }
  }

  return output;
}


QImage ImagingUtil::blur(const QImage& image, int radius) {
  // Credit Blackpawn - http://www.blackpawn.com/texts/blur/default.html
  if (radius <= 0) return image;
  
  QImage input = addBorderToImage(image, QColor(0,0,0,0), radius);
  QImage output = horizontalBlur(input, radius);
  QTransform t;
  t.rotate(90);
  output = output.transformed(t);
  output = horizontalBlur(output, radius);
  t.rotate(-180);
  output = output.transformed(t);

  return output;
}