/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <ScrollPane.h>

using namespace appetizer;


ScrollPane::ScrollPane(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  content_ = NULL;

  scrollBar_ = new ScrollBar(this->parentWindow());
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
  if (scrollBar_->scrollable()) return width() - scrollBar_->width() - scrollBarGap_;
  return width();
}


int ScrollPane::contentHeight() {
  if (!content_) return 0;
  return content_->height();
}


void ScrollPane::scrollBar_valueChanged() {
	invalidate();
}


void ScrollPane::updateDisplay() {
  GraphicsItem::updateDisplay();

  if (content_) {
    content_->setY(scrollBar_->value() * (height() - contentHeight()));
    content_->setWidth(width() - scrollBar_->width() - scrollBarGap_);
    content_->updateNow();
  } 
  
  scrollBar_->move(width() - scrollBar_->width(), 0);
  scrollBar_->setHeight(height());
  scrollBar_->setContentHeight(contentHeight());

  if (!scrollBar_->scrollable()) {
    scrollBar_->setVisible(false);
    content_->setWidth(width());
  } else {
    scrollBar_->setVisible(true);
  }
}