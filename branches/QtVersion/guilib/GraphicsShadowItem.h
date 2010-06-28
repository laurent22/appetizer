/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_GraphicsShadowItem_H
#define Appetizer_GraphicsShadowItem_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>

namespace appetizer {

class GraphicsShadowItem : public GraphicsItem {

  Q_OBJECT

public:

  GraphicsShadowItem(GraphicsWindow* parentWindow);
  ~GraphicsShadowItem();
  inline GraphicsItem* source() const { return source_; }
  void setSource(GraphicsItem* v);
  inline int blurRadius() const { return blurRadius_; }
  void setBlurRadius(int v);
  inline int xOffset() const { return xOffset_; }
  void setXOffset(int v);
  inline int yOffset() const { return yOffset_; }
  void setYOffset(int v);
  inline QColor color() const { return color_; }
  void setColor(const QColor& v);

protected:

  void updateDisplay();
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:

  GraphicsItem* source_;
  QPixmap* pixmap_;
  bool updatePixmap_;

  int blurRadius_;
  int xOffset_;
  int yOffset_;
  QColor color_;

private slots:

  void source_resized();
  void source_moved();

};

}
#endif // Appetizer_GraphicsShadowItem_H
