/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azIcon.h"
#include "azMenu.h"
#include "azShortcut.h"
#include "../MiniLaunchBar.h"


const char azIcon::className[] = "Icon";

#define method(class, name) {#name, &class::name}

Lunar<azIcon>::RegType azIcon::methods[] = {
  method(azIcon, getPopupMenu),
  method(azIcon, getShortcut),
  {0,0}
};


int azIcon::getPopupMenu(lua_State *L) {
  Lunar<azMenu>::push(L, new azMenu(renderer_->GetPopupMenu()), true);

  return 1;
}


int azIcon::getShortcut(lua_State *L) {
  FolderItem* folderItem = renderer_->GetFolderItem();
  if (!folderItem) return 0;

  FolderItem* sp = FolderItem::GetFolderItemById(renderer_->GetFolderItem()->GetId());
  Lunar<azShortcut>::push(L, new azShortcut(sp), true);

  return 1;
}