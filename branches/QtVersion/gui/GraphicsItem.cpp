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


void GraphicsItem::resize(int width, int height) {
  if (width == width_ && height == height_) return;
  width_ = width;
  height_ = height;
  resizeEvent();
  invalidate();
}


void GraphicsItem::setWidth(int width) {
  if (width == width_) return;
  width_ = width;
  resizeEvent();
  invalidate();
}


void GraphicsItem::setHeight(int height) {
  if (height == height_) return;
  height_ = height;
  resizeEvent();
  invalidate();
}


void GraphicsItem::move(int x, int y) {
  setX(x);
  setY(y);
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


void GraphicsItem::resizeEvent() {
  emit resized();
}


void GraphicsItem::mousePressEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mousePressed();
}


void GraphicsItem::mouseReleaseEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mouseReleased();
}


void GraphicsItem::mouseMoveEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mouseMoved();
}


void GraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem* /* option */, QWidget* /* widget */) {
  if (showDebugRectangle_) {
    QPen pen;
    pen.setWidth(1);
    pen.setColor(Qt::red);

    QBrush brush(QColor(255,0,0,20));

    painter->setPen(pen);
    painter->setBrush(brush);
    painter->drawRect(0, 0,width()-1, height()-1);
  }
}