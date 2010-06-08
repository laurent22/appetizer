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
  ~GraphicsItem();
  inline int width() const;
  inline int height() const;
  virtual int defaultWidth() const;
  virtual int defaultHeight() const;
  void setWidth(int width);
  void setHeight(int height);
  void resize(int width, int height);
  void move(int x, int y);
  QRectF boundingRect() const;
  void addItem(QGraphicsItem* item);
  void addItemAt(QGraphicsItem* item, int index);
  void removeItem(QGraphicsItem* item);
  inline int numChildren() const;
  QGraphicsItem* getChildAt(int index) const;
  void showDebugRectangle(bool doShow = true);
  inline void invalidate();
  inline int minWidth() const;
  inline int maxWidth() const;
  inline int minHeight() const;
  inline int maxHeight() const;
  void setMinWidth(int v);
  void setMaxWidth(int v);
  void setMinHeight(int v);
  void setMaxHeight(int v);
  virtual void updateDisplay();

signals:

  void resized();
  void mousePressed();
  void mouseReleased();
  void mouseMoved();

protected:

  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
  virtual void resizeEvent();
  void mousePressEvent(QGraphicsSceneMouseEvent* event);
  void mouseReleaseEvent(QGraphicsSceneMouseEvent* event);
  void mouseMoveEvent(QGraphicsSceneMouseEvent* event);

private:

  mutable int width_;
  mutable int height_;
  mutable int defaultWidth_;
  mutable int defaultHeight_;
  mutable int minWidth_;
  mutable int maxWidth_;
  mutable int minHeight_;
  mutable int maxHeight_;
  bool showDebugRectangle_;
  bool invalidated_;

};

}
#endif // GraphicsItem_H
