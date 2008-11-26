/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __LuaWrapper_H
#define __LuaWrapper_H


enum {
  AZ_OBJECT_APPLICATION,
  AZ_OBJECT_MAIN_FRAME,
  AZ_OBJECT_ANY_ICON
};


enum {
  AZ_EVENT_POPUP_MENU_OPENING
};


int az_print(lua_State *L);
int az_addEventListener(lua_State *L);


#endif // __LuaWrapper_H