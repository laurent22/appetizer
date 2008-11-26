/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "LuaWrapper.h"


Plugin::Plugin() {
  L = lua_open();
}


Plugin::~Plugin() {
  lua_close(L);

  std::map<std::pair<int, int>, wxArrayString*>::iterator it = eventRegister_.begin();

  for(; it != eventRegister_.end(); ++it) {
    wxArrayString* v = it->second;
    wxDELETE(v);
  }
}


void Plugin::LoadFile(const wxString& luaFilePath) {
  luaopen_base(L);
  luaopen_table(L);
  luaopen_string(L);
  luaopen_math(L);

  lua_register(L, "az_print", az_print);
  lua_register(L, "az_addEventListener", az_addEventListener);

  lua_pushinteger(L, AZ_OBJECT_ANY_ICON); lua_setglobal(L, "AZ_OBJECT_ANY_ICON");
  lua_pushinteger(L, AZ_OBJECT_APPLICATION); lua_setglobal(L, "AZ_OBJECT_APPLICATION");
  lua_pushinteger(L, AZ_OBJECT_MAIN_FRAME); lua_setglobal(L, "AZ_OBJECT_MAIN_FRAME");
  lua_pushinteger(L, AZ_EVENT_POPUP_MENU_OPENING); lua_setglobal(L, "AZ_EVENT_POPUP_MENU_OPENING");

  int error = luaL_loadfile(L, luaFilePath.mb_str());

  if (error) {
    LuaUtil::LogError(error);
    return;
  }

  error = lua_pcall(L, 0, LUA_MULTRET, 0);

  if (error) LuaUtil::LogError(error);
}


void Plugin::AddEventListener(int objectId, int eventId, const wxString& functionName) {
  wxLogDebug(_T("[Plugin] Registering event: %d %d %s"), objectId, eventId, functionName);

  std::pair<int, int> pair(objectId, eventId);

  wxArrayString* functionNames = eventRegister_[pair];

  if (!functionNames) {
    functionNames = new wxArrayString();
    eventRegister_[pair] = functionNames;
  }

  functionNames->Add(functionName);
}


void Plugin::DispatchEvent(int objectId, int eventId, LuaHostTable arguments) {
  wxLogDebug(_T("[Plugin] Dispatching event: %d %d"), objectId, eventId);

  std::pair<int, int> pair(objectId, eventId);

  wxArrayString* functionNames = eventRegister_[pair];
  if (!functionNames) return;

  for (int i = 0; i < functionNames->Count(); i++) {
    wxString n = (*functionNames)[i];

    lua_getfield(L, LUA_GLOBALSINDEX, n.mb_str());

    lua_createtable(L, arguments.size(), 0);
    int tableIndex = lua_gettop(L);

    LuaHostTable::iterator it = arguments.begin();
    for(; it != arguments.end(); ++it) {
      wxString k = it->first;
      wxString v = it->second;

      lua_pushstring(L, k.mb_str());
      lua_pushstring(L, v.mb_str());
      lua_settable(L, tableIndex);
    }

    lua_call(L, 1, 0);
  }

}


lua_State* Plugin::GetLuaState() {
  return L;
}