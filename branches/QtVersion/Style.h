/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_Style_H
#define Appetizer_Style_H

namespace appetizer {


class PaddingStyle {
 
public:

  int left;
  int right;
  int bottom;
  int top;
  int width;
  int height;
  void fromRect(const QRect& rect);

};

class IconStyle { public:
  PaddingStyle padding;
  int labelGap;
};

class BackgroundStyle { public:
  PaddingStyle padding;
};


class Style {

public:

  static IconStyle icon;
  static BackgroundStyle background;

  static void loadSkinFile(const QString& filePath);

};

}
#endif // Appetizer_Style_H
