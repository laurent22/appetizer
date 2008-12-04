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



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azIcon::className[] = "Icon";

#define method(class, name) {#name, &class::name}

Lunar<azIcon>::RegType azIcon::methods[] = {
  method(azIcon, getPopupMenu),
  method(azIcon, getShortcut),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azIcon::azIcon(FolderItemRenderer* r) {
  rendererId_ = r->GetId();
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azIcon::azIcon(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Create an instance of Shortcut instead, and add it to the option panel (optionPanel)");
}


FolderItemRenderer* azIcon::Get() const {
  return wxGetApp().GetMainFrame()->GetIconPanel()->GetFolderItemRendererById(rendererId_);
}


int azIcon::getPopupMenu(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  Lunar<azMenu>::push(L, new azMenu(Get()->GetPopupMenu()), true);

  return 1;
}


int azIcon::getShortcut(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* folderItem = Get()->GetFolderItem();
  if (!folderItem) return 0;

  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);

  return 1;
}