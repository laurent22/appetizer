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
  mouseInside_ = false;
  selectionSprite_ = NULL;

  iconSprite_ = new IconSprite();
  addItem(iconSprite_);

  setIconSize(SMALL_ICON_SIZE);
  
  setAcceptHoverEvents(true);
}


void FolderItemSprite::setIconSize(int size) {
  if (iconSize_ == size) return;
  iconSize_ = size;
  resize(size + Style::icon.padding.width, size + Style::icon.padding.height);
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


void FolderItemSprite::hoverEnterEvent(QGraphicsSceneHoverEvent* /* event */) {
  mouseInside_ = true;

  if (!selectionSprite_) {
    selectionSprite_ = new NineSliceItem();  
    selectionSprite_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Base\\IconOverlayUp.png");
    addItemAt(selectionSprite_, 0);

    selectionSpriteAnimation_ = new QPropertyAnimation(selectionSprite_, "opacity");
  }

  selectionSpriteAnimation_->stop();
  selectionSpriteAnimation_->setDuration(100);
  selectionSpriteAnimation_->setStartValue(0);
  selectionSpriteAnimation_->setEndValue(1);
  selectionSpriteAnimation_->start();

  invalidate();
}


void FolderItemSprite::hoverLeaveEvent(QGraphicsSceneHoverEvent* /* event */) {
  mouseInside_ = false;

  if (selectionSprite_) {
    selectionSpriteAnimation_->stop();
    selectionSpriteAnimation_->setDuration(400);
    selectionSpriteAnimation_->setStartValue(1);
    selectionSpriteAnimation_->setEndValue(0);
    selectionSpriteAnimation_->start();
  }

  invalidate();
}


void FolderItemSprite::updateDisplay() {
  GraphicsItem::updateDisplay();  

  if (mouseInside_ && selectionSprite_) {
    selectionSprite_->resize(width(), height());
  }

  iconSprite_->setX(Style::icon.padding.left);
  iconSprite_->setY(Style::icon.padding.top);
}