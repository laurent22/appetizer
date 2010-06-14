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


FolderItemSprite::FolderItemSprite(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  folderItemId_ = -1;
  iconSize_ = -1;
  mouseInside_ = false;
  selectionSprite_ = NULL;
  selectionSpriteAnimation_ = NULL;

  iconSprite_ = new IconSprite(this->parentWindow());
  addItem(iconSprite_);

  setIconSize(SMALL_ICON_SIZE);
  
  setAcceptHoverEvents(true);
}


FolderItemSprite::~FolderItemSprite() {
  SAFE_DELETE(selectionSpriteAnimation_);
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
    selectionSprite_ = new NineSliceItem(parentWindow());  
    selectionSprite_->loadBackgroundImage(FilePaths::GetSkinFile("IconOverlayUp.png"));
    addItemAt(selectionSprite_, 0);

    selectionSpriteAnimation_ = new QPropertyAnimation(selectionSprite_, "opacity");
  }

  selectionSpriteAnimation_->stop();
  selectionSpriteAnimation_->setDuration(100);
  selectionSpriteAnimation_->setStartValue(0);
  selectionSpriteAnimation_->setEndValue(1);
  selectionSpriteAnimation_->start(QAbstractAnimation::KeepWhenStopped);

  updateDisplay();
}


void FolderItemSprite::hoverLeaveEvent(QGraphicsSceneHoverEvent* /* event */) {
  mouseInside_ = false;

  if (selectionSprite_ && selectionSpriteAnimation_) {
    selectionSpriteAnimation_->stop();
    selectionSpriteAnimation_->setDuration(400);
    selectionSpriteAnimation_->setStartValue(1);
    selectionSpriteAnimation_->setEndValue(0);
    selectionSpriteAnimation_->start(QAbstractAnimation::KeepWhenStopped);
  }

  updateDisplay();
}


void FolderItemSprite::updateDisplay() {
  GraphicsItem::updateDisplay();  

  if (mouseInside_ && selectionSprite_) {
    selectionSprite_->resize(width(), height());
  }

  iconSprite_->move(Style::icon.padding.left, Style::icon.padding.top);
}