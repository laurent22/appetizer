/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

/**
 * This class can be used to display a context menus via the popupMenu() function of some objects.
 * Menus are built by appending menu items to them.
 *
 * @example The following code creates a new menu and append a menu item to it:
 * <listing version="3.0">
 * -- Create a new menu
 * menu = Menu:new("My menu")
 * 
 * -- Create a new menu item
 * menuItem = {}
 * 
 * -- (Required) The menu item label
 * menuItem.text = "my menu item"
 * 
 * -- (Optional) Give the item an id that can be used later on to track a menu item
 * menuItem.id = "some id"
 * 
 * -- (Optional) The "tag" property can be used to attach additional data to the menu item
 * menuItem.tag = "some extra info"
 * 
 * -- (Optional) Name of the function that is going to be called when/if the menu item is selected
 * menuItem.onClick = "menuItem_click"
 * 		
 * -- Add the menu item to the menu
 * subMenu:append(menuItem) 
 * 
 * function menuItem_click(event)
 *     trace("Menu item with id ", event.menuItemId, " and tag ", event.menuItemTag, " was clicked")
 * end
 * </listing>
 */	

#include "../stdafx.h"

#include "azMenu.h"

#include "../MiniLaunchBar.h"
#include "../ExtendedMenuItem.h"
#include "LuaUtil.h"



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************


const char azMenu::className[] = "Menu";

#define method(class, name) {#name, &class::name}

Lunar<azMenu>::RegType azMenu::methods[] = {
  method(azMenu, appendSeparator),
  method(azMenu, append),
  method(azMenu, appendSubMenu),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


std::vector<azMenu*> azMenu::createdObjects_;


void azMenu::ReleaseContent() { ownContent_ = false; }


void azMenu::OnLuaScopeClose() {
  for (int i = createdObjects_.size() - 1; i >= 0; i--) {
    azMenu* m = createdObjects_.at(i);
    if (m->IsOwningContent()) continue;

    // Set it to NULL so that any further calls to any of the menu methods will
    // fail properly (i.e. without crashing Appetizer). We don't want to
    // to keep a reference to the wxMenu because it will most likely be
    // deleted once it has been displayed. The only exception is if 
    // the plugin has created the menu (and hasn't give ownership to any
    // other objects)
    m->Set(NULL);

    // The object can now be garbage collected
    createdObjects_.erase(createdObjects_.begin() + i);
  }
}


azMenu::azMenu(wxMenu* menu) { 
  menu_ = menu;
  ownContent_ = false; 
  createdObjects_.push_back(this);
}


azMenu::~azMenu() {
  if (ownContent_) wxDELETE(menu_);
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * Constructor.
 * @param String menuLabel Menu label
 * 
 */	
azMenu::azMenu(lua_State *L) {
  wxString text = LuaUtil::ToString(L, 1);

  menu_ = new wxMenu(text);
  ownContent_ = true;

  createdObjects_.push_back(this);
}


/**
 * Appends a separator item to the menu
 * 
 */	
int azMenu::appendSeparator(lua_State *L) {
  CheckWrappedObject(L, Get());

  Get()->AppendSeparator();
  return 0;
}


/**
 * Appends a menu item to the menu
 * @param MenuItem menuItem Menu item to append to the menu
 * 
 */	
int azMenu::append(lua_State *L) {
  CheckWrappedObject(L, Get());

  wxString menuItemText = LuaUtil::GetStringFromTable(L, 1, _T("text"));  
  wxString menuItemOnClick = LuaUtil::GetStringFromTable(L, 1, _T("onClick"));
  wxString menuItemId = LuaUtil::GetStringFromTable(L, 1, _T("id"));
  wxString menuItemTag = LuaUtil::GetStringFromTable(L, 1, _T("tag"));

  menuItemText.Trim(true).Trim(false);

  if (menuItemText == wxEmptyString) luaL_error(L, "Menu item must have a label");

  ExtendedMenuItem* menuItem = new ExtendedMenuItem(Get(), wxGetApp().GetUniqueInt(), menuItemText);
  menuItem->SetMetadata(_T("plugin_menuItemId"), menuItemId);
  menuItem->SetMetadata(_T("plugin_onClick"), menuItemOnClick);
  menuItem->SetMetadataPointer(_T("plugin_luaState"), (void*)L);
  menuItem->SetMetadata(_T("plugin_menuItemTag"), menuItemTag);

  Get()->Append(menuItem);

  return 0;
}


/**
 * Appends a submenu to the menu
 * @param Menu menu Submenu to append to the menu
 *
 * @example The following code creates a new menu and append a menu item to it:
 * <listing version="3.0">
 * -- Create the submenu
 * submenu = Menu:new("My menu")
 * 
 * -- Append an item to it
 * menuItem = {}
 * menuItem.text = "submenu item"
 * submenu:append(menuItem)
 * 
 * -- Append the submenu to an existing menu
 * someMenu:appendSubMenu(submenu)
 * </listing>
 * 
 */	
int azMenu::appendSubMenu(lua_State *L) {
  azMenu* subMenu = Lunar<azMenu>::check(L, -1); 
	if (subMenu->Get()->GetTitle() == wxEmptyString) luaL_error(L, "Submenu must have a title");

  Get()->AppendSubMenu(subMenu->Get(), subMenu->Get()->GetTitle());
  subMenu->ReleaseContent(); // this menu now owns the submenu

  return 0;
}