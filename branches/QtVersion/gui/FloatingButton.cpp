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


FloatingButton::FloatingButton(GraphicsWindow* parentWindow): GraphicsButtonBase(parentWindow) {

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
  //if (Style::floatingButton.shadow) setGraphicsEffect(Style::cloneShadow(Style::floatingButton.shadow));

  QString iconFile = FilePaths::GetSkinFile("ButtonIcon_" + action() + ".png");
  if (QFile::exists(iconFile)) {
    icon_.load(iconFile);
  } else {
    icon_ = QPixmap();
  }
}


void FloatingButton::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  int yOffset = 0;
  if (state() == Over) yOffset = -1;
  if (state() == Down) yOffset = +1;

  painter->drawPixmap(0, yOffset, background_);

  if (!icon_.isNull()) {
    int targetX = floor((double)(width() - icon_.width())) / 2.0;
    int targetY = floor((double)(height() - icon_.height())) / 2.0;
    painter->drawPixmap(targetX, targetY + yOffset, icon_);
  }
}


void FloatingButton::updateDisplay() {
  GraphicsButtonBase::updateDisplay();

}