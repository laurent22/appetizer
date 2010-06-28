/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_Style_H
#define Appetizer_Style_H

#include <GraphicsShadowItem.h>
#include <MessageBoxes.h>

namespace appetizer {


class Style; // Forward declaration


class RectangleStyle { public:
  RectangleStyle();
  int left;
  int right;
  int bottom;
  int top;
  int width;
  int height;
  void fromRect(const QRect& rect);
};


class ShadowStyle { public:
  ShadowStyle();
  ShadowStyle(TiXmlHandle handle);
  void fromXml(TiXmlHandle handle);
  void applyToGraphicsShadowItem(GraphicsShadowItem* item);
  int xOffset;
  int yOffset;
  int blurRadius;
  QColor color;
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
  BackgroundStyle();
  RectangleStyle padding;
  ShadowStyle* shadow;
  QRect getContentRectangle(int width, int height) const;
};

class FloatingButtonStyle { public:
  FloatingButtonStyle();
  int hGap;
  int vGap;
  ShadowStyle* shadow;
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

  static void destroyStaticData();

  static IconStyle icon;
  static BackgroundStyle background;
  static TabStyle tab;
  static IconPanelStyle iconPanel;
  static FloatingButtonStyle floatingButton;

  static void loadSkinFile(const QString& filePath);
  static SkinMetadata getSkinMetadata(TiXmlElement* skinDocumentRoot);
  static SkinMetadata getSkinMetadata(const QString& filePath);
  static bool isSkinVersionCompatible(const QString& skinVersion);
  static QRect calculateContentRectangle(int width, int height, const RectangleStyle& padding);

};

}
#endif // Appetizer_Style_H
