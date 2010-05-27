/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <GraphicsItem.h>
using namespace appetizer;

GraphicsItem::GraphicsItem() {
  width_ = 100;
  height_ = 100;
  showDebugRectangle_ = false;
  dispatchResizeEvent_ = false;
}


void GraphicsItem::showDebugRectangle(bool doShow) {
  showDebugRectangle_ = doShow;
  invalidate();
}


int GraphicsItem::width() const {
  return width_;
}


int GraphicsItem::height() const {
  return height_;
}


void GraphicsItem::setWidth(int width) {
  if (width == width_) return;
  width_ = width;
  dispatchResizeEvent_ = true;
  invalidate();
}


void GraphicsItem::setHeight(int height) {
  if (height == height_) return;
  height_ = height;
  dispatchResizeEvent_ = true;
  invalidate();
}


QRectF GraphicsItem::boundingRect() const {
  return QRectF(0, 0, width(), height());
}


void GraphicsItem::addItem(QGraphicsItem* item) {
  item->setParentItem(this);
}


void GraphicsItem::addItemAt(QGraphicsItem* item, int index) {
  item->setParentItem(this);
  
  if (childItems().size() == 0) return;
  if (index >= childItems().size()) return;

  QGraphicsItem* sibling = childItems().at(index);
  item->stackBefore(sibling);
}


void GraphicsItem::removeItem(QGraphicsItem* item){
  childItems().removeOne(item);
}


int GraphicsItem::numChildren() const {
  return QGraphicsItem::children().size();
}


QGraphicsItem* GraphicsItem::getChildAt(int index) const {
  return childItems().at(index);
}


void GraphicsItem::invalidate() {
  update(0, 0, width(), height());
}


void GraphicsItem::onResize() {

}


void GraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  painter; option; widget; // Using variables to disable the annoying warnings

  if (showDebugRectangle_) {
    QPen pen;
    pen.setWidth(1);
    pen.setColor(Qt::red);

    QBrush brush(QColor(255,0,0,20));

    painter->setPen(pen);
    painter->setBrush(brush);
    painter->drawRect(0, 0,width()-1, height()-1);
  }

  if (dispatchResizeEvent_) {
    onResize();
    dispatchResizeEvent_ = false;
  }
}