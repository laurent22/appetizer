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
  return background_.width();
}


int FloatingButton::height() const {
  return background_.height();
}


void FloatingButton::setAction(const QString& action) {
  action_ = action;
}


void FloatingButton::applySkin() {
  background_.load(FilePaths::GetSkinFile("FloatingButtonBackground.png"));
  if (Style::floatingButton.shadow) setGraphicsEffect(Style::cloneShadow(Style::floatingButton.shadow));
}


void FloatingButton::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  painter->drawPixmap(0,0, background_);
}