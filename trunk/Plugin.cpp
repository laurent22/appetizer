/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "MiniLaunchBar.h"
#include "lua_glue/azGlobal.h"
#include "lua_glue/azApplication.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azShortcut.h"
#include "FolderItemRenderer.h"



Plugin::Plugin() {
  L = lua_open();
}


Plugin::~Plugin() {
  //lua_setgcthreshold(L, 0);
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

  int error = luaL_loadfile(L, luaFilePath.mb_str());

  if (error) {
    LuaUtil::LogError(error);
    return;
  }

  Lunar<azApplication>::Register(L);
  Lunar<azIcon>::Register(L);
  Lunar<azMenu>::Register(L);
  Lunar<azShortcut>::Register(L);
  
  lua_pushliteral(L, "appetizer");
  Lunar<azApplication>::push(L, wxGetApp().GetPluginManager()->luaApplication);
  lua_settable(L, LUA_GLOBALSINDEX);

  error = lua_pcall(L, 0, 0, 0);

  if (error) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::LoadFile"));
  }
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


void Plugin::AddEventListener(void* object, const wxString& eventName, const wxString& functionName) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  AddEventListener(object, eventId, functionName);
}


void Plugin::DispatchEvent(wxObject* senderOrGlobalHook, const wxString& eventName, LuaHostTable arguments, wxObject* sender) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  DispatchEvent(senderOrGlobalHook, eventId, arguments, sender);
}


void Plugin::DispatchEvent(wxObject* senderOrGlobalHook, int eventId, LuaHostTable arguments, wxObject* sender) {
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
    wxObject* theSender = sender ? sender : senderOrGlobalHook;
    
    FolderItemRenderer* senderAsRenderer = dynamic_cast<FolderItemRenderer*>(theSender);

    if (senderAsRenderer) {
      Lunar<azIcon>::push(L, new azIcon(senderAsRenderer), true);
    }

    lua_settable(L, tableIndex);    

    int errorCode = lua_pcall(L, 1, 0, 0);

    if (errorCode) {
      const char* errorString = lua_tostring(L, -1);
      luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::DispatchEvent"));
    }

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
  
  int errorCode = lua_pcall(L, 1, 0, 0);

  if (errorCode) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::DispatchEvent"));
  }

  return true;
}


lua_State* Plugin::GetLuaState() {
  return L;
}