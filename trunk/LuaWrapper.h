/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __LuaWrapper_H
#define __LuaWrapper_H


enum {
  azEvent_OnIconPopupMenu
};


void luaWrapper_destroy();


int azPrint(lua_State *L);
int azT(lua_State *L);
int azAddEventListener(lua_State *L);
int azGetShortcutsRoot(lua_State *L);
int azGetShortcutById(lua_State *L);
int azNewMenu(lua_State *L);

int azShortcut_GetAllGroups(lua_State *L);
int azShortcut_GetName(lua_State *L);
int azShortcut_GetId(lua_State *L);
int azShortcut_AddChild(lua_State *L);

int azIcon_GetPopupMenu(lua_State *L);
int azIcon_GetShortcut(lua_State *L);

int azMenu_Append(lua_State *L);
int azMenu_AppendSubMenu(lua_State *L);
int azMenu_AppendSeparator(lua_State *L);



#endif // __LuaWrapper_H