/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <ScrollPane.h>

using namespace appetizer;


ScrollPane::ScrollPane() {
  content_ = NULL;

  scrollBar_ = new ScrollBar();
  addItem(scrollBar_);

  QObject::connect(scrollBar_, SIGNAL(valueChanged()), this, SLOT(scrollBar_valueChanged()));

  setFlag(QGraphicsItem::ItemClipsChildrenToShape, true);
}


GraphicsItem* ScrollPane::content() {
  return content_;
}


void ScrollPane::setContent(GraphicsItem* content) {
  if (content_ && content_->parentItem()) ((GraphicsItem*)(content_->parentItem()))->removeItem(content_);
  content_ = content;
  if (content_) addItem(content_);
  invalidate();
}


int ScrollPane::contentWidth() {
  return width() - scrollBar_->width();
}


int ScrollPane::contentHeight() {
  if (!content_) return 0;
  return content_->height();
}


void ScrollPane::scrollBar_valueChanged() {
	invalidate();
}


//QPainterPath ScrollPane::shape() const {
//QPainterPath path;
//path.addEllipse(boundingRect());
//return path;
//}


void ScrollPane::updateDisplay() {
  GraphicsItem::updateDisplay();
  
  scrollBar_->move(width() - scrollBar_->width(), 0);
  scrollBar_->setHeight(height());
  scrollBar_->setContentHeight(contentHeight());

  if (content_) content_->setY(scrollBar_->value() * (height() - contentHeight()));
}