/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <Constants.h>
#include <FilePaths.h>
#include <FolderItemSprite.h>
#include <Style.h>

using namespace appetizer;


FolderItemSprite::FolderItemSprite() {
  folderItemId_ = -1;
  iconSize_ = -1;

  selectionSprite_ = new NineSliceItem();
  selectionSprite_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Base\\IconOverlayUp.png");
  addItem(selectionSprite_);

  iconSprite_ = new IconSprite();
  addItem(iconSprite_);

  setIconSize(SMALL_ICON_SIZE);
}


void FolderItemSprite::setIconSize(int size) {
  if (iconSize_ == size) return;
  iconSize_ = size;
  setWidth(size + Style::icon.padding.width);
  setHeight(size + Style::icon.padding.height);
  iconSprite_->setSize(size);
  invalidate();
}


int FolderItemSprite::iconSize() const {
  return iconSize_;
}


void FolderItemSprite::setFolderItem(int folderItemId) {
  folderItemId_ = folderItemId;
  iconSprite_->setFolderItem(folderItemId);
}


FolderItem* FolderItemSprite::folderItem() const {
  return FolderItem::getFolderItemById(folderItemId_);
}


void FolderItemSprite::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);  

  selectionSprite_->setWidth(width());
  selectionSprite_->setHeight(height());
  iconSprite_->setX(Style::icon.padding.left);
  iconSprite_->setY(Style::icon.padding.top);
}