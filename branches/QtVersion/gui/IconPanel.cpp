/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Constants.h>
#include <IconPanel.h>
#include <UserSettings.h>

using namespace appetizer;


IconPanel::IconPanel(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  rootFolderItemId_ = -1;
  contentHeight_ = 0;
  rebuildFolderItems_ = false;
  updateLayout_ = false;
  updateIconSize_ = false;
  iconSize_ = SMALL_ICON_SIZE;
}


void IconPanel::setIconSize(int iconSize) {
  if (iconSize == iconSize_) return;
  iconSize_ = iconSize;
  updateIconSize_ = true;
  invalidate();
}


int IconPanel::defaultHeight() const {
  return contentHeight_;
}


FolderItem* IconPanel::rootFolderItem() {
  return FolderItem::getFolderItemById(rootFolderItemId_);
}


void IconPanel::loadFolderItems(int rootFolderItemId) {
  rootFolderItemId_ = rootFolderItemId;
  rebuildFolderItems_ = true;
  updateLayout_ = true;
  invalidate();
}


void IconPanel::resizeEvent() {
  GraphicsItem::resizeEvent();

  updateLayout_ = true;
  invalidate();
}


void IconPanel::updateDisplay() {
  GraphicsItem::updateDisplay();  

  if (rebuildFolderItems_) {
    for (int i = 0; i < (int)folderItemRenderers_.size(); i++) {
      FolderItemSprite* r = folderItemRenderers_.at(i);
      removeItem(r);
      delete r;
    }

    folderItemRenderers_.clear();

    FolderItem* source = rootFolderItem();
    if (source) {
      for (int i = 0; i < source->numChildren(); i++) {
        FolderItem* folderItem = source->getChildAt(i);
        FolderItemSprite* r = new FolderItemSprite(parentWindow());
        r->setFolderItem(folderItem->id());
        folderItemRenderers_.push_back(r);
        addItem(r);
      }
    }

    updateIconSize_ = true;
    rebuildFolderItems_ = false;
  }

  if (updateIconSize_) {
    for (int i = 0; i < (int)folderItemRenderers_.size(); i++) {
      FolderItemSprite* r = folderItemRenderers_.at(i);
      r->setIconSize(iconSize());
    }

    updateLayout_ = true;
    updateIconSize_ = false;
  }

  if (updateLayout_) {
    int itemX = 0;
    int itemY = 0;
    int gap = 4;

    for (int i = 0; i < (int)folderItemRenderers_.size(); i++) {
      FolderItemSprite* r = folderItemRenderers_.at(i);

      if (itemX + r->width() > width()) {
        itemX = 0;
        itemY += r->height() + gap;
      }

      r->setX(itemX);
      r->setY(itemY);

      itemX += r->width() + gap;

      contentHeight_ = r->y() + r->height();
    }

    updateLayout_ = false;
  }
}