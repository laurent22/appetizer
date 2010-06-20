/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FilePaths.h>
#include <FloatingButton.h>
#include <Style.h>

using namespace appetizer;


FloatingButton::FloatingButton(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
	
}


int FloatingButton::width() const {
  if (background_.isNull()) return 22;
  QRect contentRect = Style::floatingButtonStyle.getContentRectangle(background_.width(), background_.height());
  return contentRect.width();
}


int FloatingButton::height() const {
  if (background_.isNull()) return 22;
  QRect contentRect = Style::floatingButtonStyle.getContentRectangle(background_.width(), background_.height());
  return contentRect.height();
}


void FloatingButton::setAction(const QString& action) {
  action_ = action;
}


void FloatingButton::applySkin() {
  background_.load(FilePaths::GetSkinFile("FloatingButtonBackground.png"));
}


void FloatingButton::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);
  	
  QRect contentRect = Style::floatingButtonStyle.getContentRectangle(width(), height());

  painter->drawPixmap(-contentRect.x(), -contentRect.y(), background_);
}