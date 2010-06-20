/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FilePaths.h>
#include <FloatingButton.h>

using namespace appetizer;


FloatingButton::FloatingButton(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
	
}


int FloatingButton::defaultWidth() const {
  if (background_.isNull()) return 0;
  return background_.width();
}


int FloatingButton::defaultHeight() const {
  if (background_.isNull()) return 0;
  return background_.height();
}


void FloatingButton::setAction(const QString& action) {
  action_ = action;
}


void FloatingButton::applySkin() {
  background_.load(FilePaths::GetSkinFile("FloatingButtonBackground.png"));
}


void FloatingButton::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);
  	
  painter->drawPixmap(0, 0, background_);
}