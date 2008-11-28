/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "LuaUtil.h"


wxString LuaUtil::GetErrorString(int luaError) {
  switch (luaError) {
    case 0: return _T(""); break;
    case LUA_ERRSYNTAX: return _T("Syntax error"); break;
    case LUA_ERRMEM: return _T("Memory allocation error"); break;
    case LUA_ERRFILE: return _T("Cannot open / read file"); break;
    case LUA_ERRRUN: return _T("Runtime error"); break;
    case LUA_ERRERR: return _T("Error while running the error handler function"); break;
  }

  return wxString::Format(_T("%s (%d)"), _T("Unknown error"), luaError);
}


void LuaUtil::LogError(int luaError) {
  wxLogDebug(_T("[Lua Error %d] %s"), luaError, LuaUtil::GetErrorString(luaError));
}


void LuaUtil::PushString(lua_State *L, const wxString& s) {
  lua_pushstring(L, s.ToUTF8());
}


wxString LuaUtil::ToString(lua_State *L, int n) {
  wxString output(luaL_checkstring(L, n), wxConvUTF8);
  return output;
}


bool LuaUtil::ToBoolean(lua_State *L, int n) {
  if (!lua_isboolean(L, n)) return false;
  return lua_toboolean(L, n);
}


wxString LuaUtil::GetStringFromTable(lua_State *L, int tableIndex, const wxString& key) {
  lua_pushstring(L, key.mb_str());
  lua_gettable(L, tableIndex);
  wxString output = LuaUtil::ToString(L, -1);
  lua_pop(L, 1);

  return output;
}