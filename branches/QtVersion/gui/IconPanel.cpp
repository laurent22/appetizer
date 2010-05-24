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
}


FolderItem* IconPanel::rootFolderItem() {
  return FolderItem::getFolderItemById(rootFolderItemId_);
}


void IconPanel::loadFolderItems(int rootFolderItemId) {
  rootFolderItemId_ = rootFolderItemId;
}


void IconPanel::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {

}