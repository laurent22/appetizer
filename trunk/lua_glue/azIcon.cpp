/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * This class represents and icon, as displayed on the dock. It cannot be directly
 * created in Lua, however it is possible to indirectly create a new icon by creating a DockItem
 * and adding it to the <code>optionPanel</code> global object. Some events also return a reference
 * to an Icon object.
 *
 * @see DockItem
 *
 */	


#include "../stdafx.h"

#include "azIcon.h"
#include "azMenu.h"
#include "azDockItem.h"
#include "../MiniLaunchBar.h"



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azIcon::className[] = "Icon";

#define method(class, name) {#name, &class::name}

Lunar<azIcon>::RegType azIcon::methods[] = {
  method(azIcon, getDockItem),
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
  luaL_error(L, "This object cannot be directly created. Create an instance of DockItem instead, and add it to the option panel (optionPanel)");
}


/**
 * Returns the dock item associated with this icon.
 * @return DockItem The icon's dock item
 * @example The following code displays the dock item's path associated with the icon:
 * <listing version="3.0">
 * dockItem = icon:getDockItem()
 * trace("This icon links to ", dockItem:getPath())
 * </listing>
 * 
 */	
int azIcon::getDockItem(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* folderItem = Get()->GetFolderItem();
  if (!folderItem) luaL_error(L, "Cannot get associated dock item");

  Lunar<azDockItem>::push(L, new azDockItem(folderItem), true);

  return 1;
}