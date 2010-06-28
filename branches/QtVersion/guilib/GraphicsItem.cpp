/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <GraphicsItem.h>
using namespace appetizer;

GraphicsItem::GraphicsItem(GraphicsWindow* parentWindow): QGraphicsItem() {
  parentWindow_ = parentWindow;
  minWidth_ = INT_MIN;
  minHeight_ = INT_MIN;
  maxWidth_ = INT_MAX;
  maxHeight_ = INT_MAX;
  width_ = -1;
  height_ = -1;
  defaultWidth_ = 100;
  defaultHeight_ = 20;
  showDebugRectangle_ = false;
  invalidated_ = false;

  invalidate();
}


void GraphicsItem::setOpacity(qreal opacity) {
  QGraphicsItem::setOpacity(opacity);

  parentWindow()->invalidateDisplay();
}


GraphicsItem::~GraphicsItem() {
  for (int i = numChildren() - 1; i >= 0; i--) {
    QGraphicsItem* item = getChildAt(i);
    removeItem(item);
    SAFE_DELETE(item);
  }

  GraphicsItem* parent = static_cast<GraphicsItem*>(parentItem());
  if (parent) parent->removeItem(this);
}


int GraphicsItem::minWidth() const { return minWidth_; }
int GraphicsItem::maxWidth() const { return maxWidth_; }
int GraphicsItem::minHeight() const { return minHeight_; }
int GraphicsItem::maxHeight() const { return maxHeight_; }
void GraphicsItem::setMinWidth(int v) { if (minWidth_ == v) return; minWidth_ = v; invalidate(); }
void GraphicsItem::setMaxWidth(int v) { if (maxWidth_ == v) return; maxWidth_ = v; invalidate(); }
void GraphicsItem::setMinHeight(int v) { if (minHeight_ == v) return; minHeight_ = v; invalidate(); }
void GraphicsItem::setMaxHeight(int v) { if (maxHeight_ == v) return; maxHeight_ = v; invalidate(); }


void GraphicsItem::showDebugRectangle(bool doShow) {
  showDebugRectangle_ = doShow;
  invalidate();
}


int GraphicsItem::defaultWidth() const {
  return defaultWidth_;
}


int GraphicsItem::defaultHeight() const {
  return defaultHeight_;
}


int GraphicsItem::width() const {
  int output = width_;
  if (output < 0) output = defaultWidth();
  if (output < minWidth()) return minWidth();
  if (output > maxWidth()) return maxWidth();
  return output;
}


int GraphicsItem::height() const {
  int output = height_;
  if (output < 0) output = defaultHeight();
  if (output < minHeight()) return minHeight();
  if (output > maxHeight()) return maxHeight();
  return output;
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
  if (this->x() == x && this->y() == y) return;

  QGraphicsItem::setX(x);
  QGraphicsItem::setY(y);
  moveEvent();
}


void GraphicsItem::setX(qreal x) {
  if (this->x() == x) return;

  QGraphicsItem::setX(x);
  moveEvent();
}


void GraphicsItem::setY(qreal y) {
  if (this->y() == y) return;

  QGraphicsItem::setY(y);
  moveEvent();
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
  item->setParentItem(NULL);
  if (item->scene()) item->scene()->removeItem(item);
}


int GraphicsItem::numChildren() const {
  return QGraphicsItem::children().size();
}


QGraphicsItem* GraphicsItem::getChildAt(int index) const {
  return childItems().at(index);
}


void GraphicsItem::invalidate() {
  invalidated_ = true;
  update(0, 0, width(), height());
}


void GraphicsItem::moveEvent() {
  emit moved();
  parentWindow()->invalidateDisplay();
}


void GraphicsItem::resizeEvent() {
  emit resized();
  invalidate();
}


void GraphicsItem::mousePressEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mousePressed();
}


void GraphicsItem::mouseReleaseEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mouseReleased();
  if (isUnderMouse()) emit clicked();
}


void GraphicsItem::mouseMoveEvent(QGraphicsSceneMouseEvent* /* event */) {
  emit mouseMoved();
}


void GraphicsItem::updateNow(bool onlyIfInvalidated) {
  if (onlyIfInvalidated && !invalidated_) return;

  invalidated_ = false;
  updateDisplay();
  parentWindow()->invalidateDisplay();
}


void GraphicsItem::updateDisplay() {

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

  if (invalidated_) {
    invalidated_ = false;
    updateDisplay();
    parentWindow()->invalidateDisplay();
  }
}