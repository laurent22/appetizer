/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <IconPanel.h>

using namespace appetizer;


IconPanel::IconPanel() {
  rootFolderItemId_ = -1;
  contentHeight_ = 0;
  rebuildFolderItems_ = false;
  updateLayout_ = false;
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
        FolderItemSprite* r = new FolderItemSprite();
        r->setFolderItem(folderItem->id());
        folderItemRenderers_.push_back(r);

        r->setIconSize(40);

        addItem(r);
      }
    }

    rebuildFolderItems_ = false;
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