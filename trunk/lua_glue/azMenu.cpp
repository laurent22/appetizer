/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azMenu.h"

#include "../MiniLaunchBar.h"
#include "../ExtendedMenuItem.h"
#include "LuaUtil.h"


const char azMenu::className[] = "azMenu";

#define method(class, name) {#name, &class::name}

Lunar<azMenu>::RegType azMenu::methods[] = {
  method(azMenu, appendSeparator),
  method(azMenu, append),
  method(azMenu, appendSubMenu),
  {0,0}
};


void azMenu::SetOwnContent(bool v) { ownContent_ = v; }


azMenu::azMenu(wxMenu* menu) { 
  menu_ = menu;
  ownContent_ = false; 
}


azMenu::azMenu(lua_State *L) {
  wxString text = LuaUtil::ToString(L, 1);

  menu_ = new wxMenu(text);
  ownContent_ = true;
}


azMenu::~azMenu() {
  if (ownContent_) wxDELETE(menu_);
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
  azMenu* subMenu = Lunar<azMenu>::check(L, -1); 
	if (subMenu->Get()->GetTitle() == wxEmptyString) return 0;

  menu_->AppendSubMenu(subMenu->Get(), subMenu->Get()->GetTitle());
  subMenu->SetOwnContent(false); // menu_ now owns the submenu

  return 0;
}