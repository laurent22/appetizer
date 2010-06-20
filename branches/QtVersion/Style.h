/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_Style_H
#define Appetizer_Style_H

#include <MessageBoxes.h>

namespace appetizer {


class Style; // Forward declaration


class RectangleStyle { public:
  int left;
  int right;
  int bottom;
  int top;
  int width;
  int height;
  void fromRect(const QRect& rect);
};

class TextFormat { public:
  TextFormat();
  void fromXml(TiXmlHandle handle);

  QColor color;
  int size;
  bool bold;
  QString family;
  QFont font();

  private:

    bool fontInitialized_;
    QFont font_;
};

class IconStyle { public:
  RectangleStyle padding;
  int labelGap;
};

class BackgroundStyle { public:
  RectangleStyle padding;
  RectangleStyle shadowPadding;
  QRect getContentRectangle(int width, int height) const;
};

class IconPanelStyle { public:
  RectangleStyle padding;
};

class TabStyle { public:
  RectangleStyle padding;
  int hGap;
  TextFormat textFormat;
};

struct SkinMetadata {
  QString name;
  QString compatibleVersion;
  QString author;
  bool isNull;
};


class Style {

public:

  static IconStyle icon;
  static BackgroundStyle background;
  static TabStyle tab;
  static IconPanelStyle iconPanel;

  static void loadSkinFile(const QString& filePath);
  static SkinMetadata getSkinMetadata(TiXmlElement* skinDocumentRoot);
  static SkinMetadata getSkinMetadata(const QString& filePath);
  static bool isSkinVersionCompatible(const QString& skinVersion);
  static QRect calculateContentRectangle(int width, int height, const RectangleStyle& shadowPadding, const RectangleStyle& padding);

};

}
#endif // Appetizer_Style_H
