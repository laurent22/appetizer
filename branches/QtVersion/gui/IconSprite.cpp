/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <Constants.h>
#include <IconSprite.h>
using namespace appetizer;


IconSprite::IconSprite() {
  folderItemId_ = -1;
  size_ = LARGE_ICON_SIZE;
}


FolderItem* IconSprite::folderItem() const {
  return FolderItem::getFolderItemById(folderItemId_);
}


void IconSprite::setFolderItem(int folderItemId) {
  folderItemId_ = folderItemId;
  invalidate();
}


int IconSprite::size() const {
  return size_;
}


void IconSprite::setSize(int size) {
  size_ = size;
  setWidth(size);
  setHeight(size);
  invalidate();
}


void IconSprite::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  FolderItem* f = folderItem();
  if (!f) return;

  QPixmap* icon = f->getIconPixmap(Application::instance()->getNextValidIconSize(size()));
  if (!icon) return;

  painter->setRenderHint(QPainter::SmoothPixmapTransform);
  painter->drawPixmap(QRect(0, 0, width(), height()), *icon);
}