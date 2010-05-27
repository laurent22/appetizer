/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef GraphicsItem_H
#define GraphicsItem_H
namespace appetizer {

class GraphicsItem : public QObject, public QGraphicsItem {

  Q_OBJECT
  Q_PROPERTY(qreal opacity READ opacity WRITE setOpacity)

public:

  GraphicsItem();
  int width() const;
  int height() const;
  void setWidth(int width);
  void setHeight(int height);
  QRectF boundingRect() const;
  void addItem(QGraphicsItem* item);
  void addItemAt(QGraphicsItem* item, int index);
  void removeItem(QGraphicsItem* item);
  int numChildren() const;
  QGraphicsItem* getChildAt(int index) const;
  void showDebugRectangle(bool doShow = true);
  void invalidate();

  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

protected:

  virtual void onResize();

private:

  int width_;
  int height_;
  bool showDebugRectangle_;
  bool dispatchResizeEvent_;

};

}
#endif // GraphicsItem_H
