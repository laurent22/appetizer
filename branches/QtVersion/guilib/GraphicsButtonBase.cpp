/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <GraphicsButtonBase.h>

using namespace appetizer;


GraphicsButtonBase::GraphicsButtonBase(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  setState(Up);
  setAcceptHoverEvents(true);
  setCursor(Qt::PointingHandCursor);
}


void GraphicsButtonBase::setState(ButtonState v) {
  if (v == state_) return;
  state_ = v;
  invalidate();
}


void GraphicsButtonBase::mousePressEvent(QGraphicsSceneMouseEvent* event) {
  setState(Down);
  updateNow();
  GraphicsItem::mousePressEvent(event);
}


void GraphicsButtonBase::mouseReleaseEvent(QGraphicsSceneMouseEvent* event) {
  setState(Up);
  updateNow();
  GraphicsItem::mouseReleaseEvent(event);
}


void GraphicsButtonBase::hoverEnterEvent(QGraphicsSceneHoverEvent* /* event */) {
  setState(Over);
  updateNow();
}


void GraphicsButtonBase::hoverLeaveEvent(QGraphicsSceneHoverEvent* /* event */) {
  setState(Up);
  updateNow();
}