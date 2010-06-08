/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <TabSprite.h>
#include <Style.h>

using namespace appetizer;

TabSprite::TabSprite() {
  backgroundSprite_ = new NineSliceItem();
  backgroundSprite_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Base\\IconOverlayUp.png");
  addItem(backgroundSprite_);

  textSprite_ = new QGraphicsSimpleTextItem();
  addItem(textSprite_);

  applySkin();
}


void TabSprite::applySkin() {
  textSprite_->setFont(Style::tab.textFormat.font());
  textSprite_->setBrush(QBrush(Style::tab.textFormat.color));

  invalidate();
}


void TabSprite::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  backgroundSprite_->resize(width(), height());

  textSprite_->setText("First tab");

  QRectF r = textSprite_->boundingRect();
  int y = floor((height() - r.height()) / 2);
  textSprite_->setX(10);
  textSprite_->setY(y);
}