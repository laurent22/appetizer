/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azShortcut.h"

#include "../MiniLaunchBar.h"


const char azShortcut::className[] = "azShortcut";

#define method(class, name) {#name, &class::name}

Lunar<azShortcut>::RegType azShortcut::methods[] = {
  method(azShortcut, getAllGroups),
  method(azShortcut, getName),
  method(azShortcut, getId),
  method(azShortcut, addChild),
  {0,0}
};


int azShortcut::getAllGroups(lua_State *L) {
  FolderItemVector allGroups = folderItem_->GetAllGroups();

  lua_createtable(L, allGroups.size(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < allGroups.size(); i++) {
    FolderItemSP f = allGroups.at(i);

    lua_pushinteger(L, i + 1);
    Lunar<azShortcut>::push(L, new azShortcut(f), true);
    lua_settable(L, tableIndex);
  }

  return 1;
}


int azShortcut::getName(lua_State *L) {
  lua_pushstring(L, folderItem_->GetName(true).ToUTF8());
  
  return 1;
}


int azShortcut::getId(lua_State *L) {
  lua_pushinteger(L, folderItem_->GetId());
  
  return 1;
}


int azShortcut::addChild(lua_State *L) {
  const azShortcut* shortcut = Lunar<azShortcut>::check(L, -1); 
  if (!shortcut) return 0;

  folderItem_->AddChild(shortcut->folderItem_);

  wxGetApp().FolderItems_CollectionChange();

  return 0;
}