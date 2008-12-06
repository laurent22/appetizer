/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * Menu items are used in conjunction with the Menu class to build menus. 
 * @see Menu
 */	


#include "../stdafx.h"

#include "azMenuItem.h"
#include "LuaUtil.h"



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azMenuItem::className[] = "MenuItem";

#define method(class, name) {#name, &class::name}

Lunar<azMenuItem>::RegType azMenuItem::methods[] = {
  method(azMenuItem, getText),
  method(azMenuItem, setText),
  method(azMenuItem, getId),
  method(azMenuItem, setId),
  method(azMenuItem, getTag),
  method(azMenuItem, setTag),
  method(azMenuItem, getOnSelected),
  method(azMenuItem, setOnSelected),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azWrappedMenuItem* azMenuItem::Get() const {
  return menuItem_;
}


azMenuItem::~azMenuItem() {
  wxDELETE(menuItem_);
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * Constructor.
 * @param String text The text (label) of the menu item.
 */
azMenuItem::azMenuItem(lua_State *L) {
  wxString text = LuaUtil::ToString(L, 1);
  text.Trim(true).Trim(false);
  if (text == wxEmptyString) luaL_error(L, "The menu item must have a non-empty label");

  menuItem_ = new azWrappedMenuItem();
  menuItem_->text = text;
}


/**
 * Gets the text (label) property of the menu item.
 * @return String Menu item text
 */
int azMenuItem::getText(lua_State *L) { LuaUtil::PushString(L, Get()->text); return 1; }
/**
 * Sets the text (label) property of the menu item.
 * @param String text Menu item text
 */
int azMenuItem::setText(lua_State *L) { Get()->text = LuaUtil::ToString(L, 1); return 0; }
/**
 * Gets the id of the menu item
 * @return String Menu item ID
 */
int azMenuItem::getId(lua_State *L) { LuaUtil::PushString(L, Get()->id); return 1; }
/**
 * Sets the id of the menu item. The menu item ID is user-defined and can be any string.
 * @param String id Menu item ID
 */
int azMenuItem::setId(lua_State *L) { Get()->id = LuaUtil::ToString(L, 1); return 0; }
/**
 * Gets the tag property of the menu item
 * @return String Menu item tag
 */
int azMenuItem::getTag(lua_State *L) { LuaUtil::PushString(L, Get()->tag); return 1; }
/**
 * Sets the tag property of the menu item. The menu item tag is user-defined and can be any string. It can be used
 * to associate some additional data, such as the ID of an object, to the menu item.
 * @param String tag Menu item tag
 */
int azMenuItem::setTag(lua_State *L) { Get()->tag = LuaUtil::ToString(L, 1); return 0; }
/**
 * Gets the "onSelected" event handler.
 * @return String Name of the menu item event handler
 */
int azMenuItem::getOnSelected(lua_State *L) { LuaUtil::PushString(L, Get()->onSelected); return 1; }
/**
 * Sets the "onSelected" event handler. The associated function will be called when the user selects
 * this particular menu item.
 * @param String functionName Name of the menu item event handler
 * @example The examples below creates a menu, associate a menu item with it, and displays a message when the item is clicked.
 * <listing version="3.0">
 * -- Create a new menu
 * menu = Menu:new("My menu")
 * 
 * -- Create a new menu item
 * menuItem = MenuItem:new("my menu item")
 * 
 * -- Set the menu item event handler
 * menuItem:setOnSelected("menuItem_selected")
 *
 * -- This function will be called when the menu item is clicked:
 * function menuItem_selected(event)
 *     dialogs:showMessage("'"..event.menuItem:getText().."' was clicked!")
 * end
 * </listing>
 */
int azMenuItem::setOnSelected(lua_State *L) { Get()->onSelected = LuaUtil::ToString(L, 1); return 0; }