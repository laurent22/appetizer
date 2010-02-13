/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azWrapper.h"



int azWrapper::LuaNullError(lua_State *L) {
  return luaL_error(L, "Object is null or has been deleted.");
}


int azWrapper::LuaPrivateConstructorError(lua_State *L) {
  return luaL_error(L, "This object cannot be directly created");
}


int azWrapper::CheckWrappedObject(lua_State *L, wxObject* wrappedObject) {
  if (!wrappedObject) return LuaNullError(L);
  return 0;
}