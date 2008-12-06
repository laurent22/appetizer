/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * This class represents and icon, as displayed on the dock. It cannot be directly
 * created in Lua, however it is possible to indirectly create a new icon by creating a Shortcut
 * and adding it to the <code>optionPanel</code> global object. Some events also return a reference
 * to an Icon object.
 *
 * @see Shortcut
 *
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


FolderItemRenderer* azIcon::Get() const {
  return wxGetApp().GetMainFrame()->GetIconPanel()->GetFolderItemRendererById(rendererId_);
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azIcon::azIcon(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Create an instance of Shortcut instead, and add it to the option panel (optionPanel)");
}


/**
 * Returns the popup menu associated with this icon.
 * @return Menu The icon popup menu
 * 
 */	
int azIcon::getPopupMenu(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  Lunar<azMenu>::push(L, new azMenu(Get()->GetPopupMenu()), true);

  return 1;
}


/**
 * Returns the shortcut object associated with this icon.
 * @return Shortcut The icon's shortcut
 * @example The following code displays the shortcut path associated with the icon:
 * <listing version="3.0">
 * shortcut = icon:getShortcut()
 * trace("This icon links to ", shortcut:getPath())
 * </listing>
 * 
 */	
int azIcon::getShortcut(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* folderItem = Get()->GetFolderItem();
  if (!folderItem) luaL_error(L, "Cannot get associated shortcut");

  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);

  return 1;
}