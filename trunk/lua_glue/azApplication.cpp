/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azApplication.h"
#include "azShortcut.h"
#include "../MiniLaunchBar.h"


const char azApplication::className[] = "azApplication";

#define method(class, name) {#name, &class::name}

Lunar<azApplication>::RegType azApplication::methods[] = {
  method(azApplication, addEventListener),
  method(azApplication, getShortcutRoot),
  method(azApplication, getShortcutById),
  {0,0}
};


int azApplication::addEventListener(lua_State *L) {
  wxString eventName = LuaUtil::ToString(L, 1);
  wxString functionName = LuaUtil::ToString(L, 2);

  PluginSP p = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  p->AddEventListener(this, eventName, functionName);

  return 0;
}


int azApplication::getShortcutRoot(lua_State *L) {
  FolderItemSP rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  Lunar<azShortcut>::push(L, new azShortcut(rootFolderItem), true);

  return 1;
}


int azApplication::getShortcutById(lua_State *L) {
  int folderItemId = luaL_checkinteger(L, 1);
  FolderItemSP folderItem = FolderItem::GetFolderItemById(folderItemId);
  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);
  
  return 1;
}