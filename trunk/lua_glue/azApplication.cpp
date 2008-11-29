/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azApplication.h"
#include "azShortcut.h"
#include "azGlobal.h"
#include "../MiniLaunchBar.h"


const char azApplication::className[] = "azApplication";

#define method(class, name) {#name, &class::name}

Lunar<azApplication>::RegType azApplication::methods[] = {
  method(azApplication, addEventListener),
  method(azApplication, getShortcutRoot),
  method(azApplication, getShortcutById),
  {0,0}
};


MiniLaunchBar* azApplication::Get() const { return &(wxGetApp()); }


azIMPLEMENT_EVENT_LISTENER_FUNCTION(azApplication)


int azApplication::getShortcutRoot(lua_State *L) {
  FolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  Lunar<azShortcut>::push(L, new azShortcut(rootFolderItem), true);

  return 1;
}


int azApplication::getShortcutById(lua_State *L) {
  int folderItemId = luaL_checkinteger(L, 1);
  FolderItem* folderItem = FolderItem::GetFolderItemById(folderItemId);
  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);
  
  return 1;
}