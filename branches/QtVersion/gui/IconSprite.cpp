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


void IconSprite::folderItem_iconLoaded(int /* iconSize */) {
  FolderItem* f = folderItem();
  if (f) QObject::disconnect(f, SIGNAL(iconLoaded(int)), this, SLOT(folderItem_iconLoaded(int)));

  invalidate();
}


void IconSprite::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  FolderItem* f = folderItem();
  if (!f) return;

  int iconSize = Application::instance()->getNextValidIconSize(size());
  int loadingState = f->iconDataLoadingState(iconSize);

  if (loadingState == ICON_LOADING_STATE_LOADED) {

    QPixmap* icon = f->getIconPixmap(iconSize);
    if (!icon) return;
    painter->setRenderHint(QPainter::SmoothPixmapTransform);
    painter->drawPixmap(QRect(0, 0, width(), height()), *icon);

  } else if (loadingState == ICON_LOADING_STATE_UNLOADED) {

    QObject::connect(f, SIGNAL(iconLoaded(int)), this, SLOT(folderItem_iconLoaded(int)));
    f->loadIconData(iconSize);

  } else if (loadingState == ICON_LOADING_STATE_ERROR) {

    qDebug() << "Error loading icon";

  }

}