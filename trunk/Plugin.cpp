/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "LuaWrapper.h"
#include "MiniLaunchBar.h"



Plugin::Plugin() {
  L = lua_open();
}


Plugin::~Plugin() {
  lua_close(L);

  std::map<std::pair<void*, int>, wxArrayString*>::iterator it = eventRegister_.begin();

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

  lua_register(L, "azPrint", azPrint);
  lua_register(L, "azT", azT);
  lua_register(L, "azAddEventListener", azAddEventListener);
  lua_register(L, "azGetShortcutsRoot", azGetShortcutsRoot);
  lua_register(L, "azNewMenu", azNewMenu);
  lua_register(L, "azGetShortcutById", azGetShortcutById);  

  lua_register(L, "azShortcut_GetAllGroups", azShortcut_GetAllGroups);
  lua_register(L, "azShortcut_GetName", azShortcut_GetName);
  lua_register(L, "azShortcut_GetId", azShortcut_GetId);
  lua_register(L, "azShortcut_AddChild", azShortcut_AddChild);

  lua_register(L, "azIcon_GetPopupMenu", azIcon_GetPopupMenu);
  lua_register(L, "azIcon_GetShortcut", azIcon_GetShortcut);

  lua_register(L, "azMenu_Append", azMenu_Append);
  lua_register(L, "azMenu_AppendSubMenu", azMenu_AppendSubMenu);
  lua_register(L, "azMenu_AppendSeparator", azMenu_AppendSeparator);

  lua_pushinteger(L, azEvent_OnIconPopupMenu); lua_setglobal(L, "azEvent_OnIconPopupMenu");
  lua_pushlightuserdata(L, &(wxGetApp())); lua_setglobal(L, "azApp");

  int error = luaL_loadfile(L, luaFilePath.mb_str());

  if (error) {
    LuaUtil::LogError(error);
    return;
  }

  error = lua_pcall(L, 0, LUA_MULTRET, 0);

  if (error) LuaUtil::LogError(error);
}


void Plugin::AddEventListener(void* object, int eventId, const wxString& functionName) {
  std::pair<void*, int> pair(object, eventId);

  wxArrayString* functionNames = eventRegister_[pair];

  if (!functionNames) {
    functionNames = new wxArrayString();
    eventRegister_[pair] = functionNames;
  }

  functionNames->Add(functionName);
}


void Plugin::DispatchEvent(void* senderOrGlobalHook, int eventId, LuaHostTable arguments, void* sender) {
  std::pair<void*, int> pair(senderOrGlobalHook, eventId);

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

    lua_pushstring(L, "sender");
    void* theSender = sender ? sender : senderOrGlobalHook;
    lua_pushlightuserdata(L, theSender);

    lua_settable(L, tableIndex);

    lua_call(L, 1, 0);
  }

}


bool Plugin::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  wxString onClickHandler = menuItem->GetMetadata(_T("plugin_onClick"));
  if (onClickHandler == wxEmptyString) return false;

  lua_getfield(L, LUA_GLOBALSINDEX, onClickHandler.mb_str());

  lua_createtable(L, 1, 0);
  int tableIndex = lua_gettop(L);

  lua_pushstring(L, "menuItemId");
  lua_pushstring(L, menuItem->GetMetadata(_T("plugin_menuItemId")).mb_str());
  lua_settable(L, tableIndex);

  lua_pushstring(L, "menuItemTag");
  lua_pushstring(L, menuItem->GetMetadata(_T("plugin_menuItemTag")).mb_str());
  lua_settable(L, tableIndex);  
  
  lua_call(L, 1, 0);

  return true;
}


lua_State* Plugin::GetLuaState() {
  return L;
}