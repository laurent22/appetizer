/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_ImagingUtil_H
#define Appetizer_ImagingUtil_H

namespace appetizer {


class ImagingUtil {

public:

  static QImage addBorderToImage(const QImage& image, const QColor& color, int left, int top = -1, int right = -1, int bottom = -1);
  static QImage horizontalBlur(const QImage& image, int radius);
  static QImage blur(const QImage& image, int radius);
  static QImage fillKeepAlpha(const QImage& image, const QColor& color);

};

}
#endif // Appetizer_ImagingUtil_H
