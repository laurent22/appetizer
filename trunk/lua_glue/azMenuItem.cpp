/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
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


azMenuItem::azMenuItem(lua_State *L) {
  wxString text = LuaUtil::ToString(L, 1);
  text.Trim(true).Trim(false);
  if (text == wxEmptyString) luaL_error(L, "The menu item must have a non-empty label");

  menuItem_ = new azWrappedMenuItem();
  menuItem_->text = text;
}


int azMenuItem::getText(lua_State *L) { LuaUtil::PushString(L, Get()->text); return 1; }
int azMenuItem::setText(lua_State *L) { Get()->text = LuaUtil::ToString(L, 1); return 0; }
int azMenuItem::getId(lua_State *L) { LuaUtil::PushString(L, Get()->id); return 1; }
int azMenuItem::setId(lua_State *L) { Get()->id = LuaUtil::ToString(L, 1); return 0; }
int azMenuItem::getTag(lua_State *L) { LuaUtil::PushString(L, Get()->tag); return 1; }
int azMenuItem::setTag(lua_State *L) { Get()->tag = LuaUtil::ToString(L, 1); return 0; }
int azMenuItem::getOnSelected(lua_State *L) { LuaUtil::PushString(L, Get()->onSelected); return 1; }
int azMenuItem::setOnSelected(lua_State *L) { Get()->onSelected = LuaUtil::ToString(L, 1); return 0; }