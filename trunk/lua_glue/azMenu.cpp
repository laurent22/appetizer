/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azMenu.h"

#include "../MiniLaunchBar.h"
#include "../ExtendedMenuItem.h"
#include "../utilities/LuaUtil.h"


const char azMenu::className[] = "azMenu";

#define method(class, name) {#name, &class::name}

Lunar<azMenu>::RegType azMenu::methods[] = {
  method(azMenu, appendSeparator),
  method(azMenu, append),
  method(azMenu, appendSubMenu),
  {0,0}
};


azMenu::azMenu(lua_State *L) {
  wxString text = LuaUtil::ToString(L, 1);

  menu_ = new wxMenu(text);
}  


int azMenu::appendSeparator(lua_State *L) {
  menu_->AppendSeparator();
  return 0;
}


int azMenu::append(lua_State *L) {
  wxString menuItemText = LuaUtil::GetStringFromTable(L, 1, _T("text"));  
  wxString menuItemOnClick = LuaUtil::GetStringFromTable(L, 1, _T("onClick"));
  wxString menuItemId = LuaUtil::GetStringFromTable(L, 1, _T("id"));
  wxString menuItemTag = LuaUtil::GetStringFromTable(L, 1, _T("tag"));

  menuItemText.Trim(true).Trim(false);

  if (menuItemText == wxEmptyString) return 0;

  ExtendedMenuItem* menuItem = new ExtendedMenuItem(menu_, wxGetApp().GetUniqueInt(), menuItemText);
  menuItem->SetMetadata(_T("plugin_menuItemId"), menuItemId);
  menuItem->SetMetadata(_T("plugin_onClick"), menuItemOnClick);
  menuItem->SetMetadataPointer(_T("plugin_luaState"), (void*)L);
  menuItem->SetMetadata(_T("plugin_menuItemTag"), menuItemTag);

  menu_->Append(menuItem);

  return 0;
}


int azMenu::appendSubMenu(lua_State *L) {
  const azMenu* subMenu = Lunar<azMenu>::check(L, -1); 
  if (!subMenu) return 0;

  menu_->AppendSubMenu(subMenu->menu_, subMenu->menu_->GetTitle());

  return 0;
}