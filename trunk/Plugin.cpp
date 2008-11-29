/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "MiniLaunchBar.h"
#include "FolderItemRenderer.h"
#include "OptionButton.h"

#include "lua_glue/azGlobal.h"
#include "lua_glue/azApplication.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azOptionPanel.h"
#include "lua_glue/azOptionButton.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azShortcut.h"



Plugin::Plugin() {
  L = lua_open();
}


Plugin::~Plugin() {
  lua_close(L);

  std::map<std::pair<wxObject*, int>, wxArrayString*>::iterator it = eventRegister_.begin();

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
  Lunar<azOptionButton>::Register(L);
  Lunar<azOptionPanel>::Register(L);
  
  lua_pushliteral(L, "appetizer");
  Lunar<azApplication>::push(L, wxGetApp().GetPluginManager()->luaApplication);
  lua_settable(L, LUA_GLOBALSINDEX);

  lua_pushliteral(L, "optionPanel");
  Lunar<azOptionPanel>::push(L, wxGetApp().GetPluginManager()->luaOptionPanel);
  lua_settable(L, LUA_GLOBALSINDEX);

  error = lua_pcall(L, 0, 0, 0);

  if (error) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::LoadFile"));
  }
}


void Plugin::AddEventListener(wxObject* object, int eventId, const wxString& functionName) {
  std::pair<wxObject*, int> pair(object, eventId);

  wxArrayString* functionNames = eventRegister_[pair];

  if (!functionNames) {
    functionNames = new wxArrayString();
    eventRegister_[pair] = functionNames;
  }

  functionNames->Add(functionName);
}


void Plugin::AddEventListener(wxObject* object, const wxString& eventName, const wxString& functionName) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  AddEventListener(object, eventId, functionName);
}


void Plugin::DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable arguments) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  DispatchEvent(sender, eventId, arguments);
}


template <class T>
bool luaPushAsWrapper(lua_State* L, boost::any o) {
  T* asType = boost::any_cast<T*>(o);
  if (asType) {
    Lunar<T>::push(L, asType, true);
    return true;
  }
  return false;
}


template <class hostObjectT, class lunarObjectT>
bool luaConvertAndPushAsWrapper(lua_State* L, wxObject* o) {
  hostObjectT* asType = dynamic_cast<hostObjectT*>(o);
  if (asType) {
    Lunar<lunarObjectT>::push(L, new lunarObjectT(asType), true);
    return true;
  }
  return false;
}


void Plugin::DispatchEvent(wxObject* sender, int eventId, LuaHostTable arguments) {
  std::pair<wxObject*, int> pair(sender, eventId);

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
      boost::any v = it->second;

      lua_pushstring(L, k.mb_str());

      bool done = false;

      done = luaPushAsWrapper<azIcon>(L, v);
      if (!done) done = luaPushAsWrapper<azMenu>(L, v);
      if (!done) done = luaPushAsWrapper<azOptionButton>(L, v);
      if (!done) done = luaPushAsWrapper<azOptionPanel>(L, v);
      if (!done) done = luaPushAsWrapper<azShortcut>(L, v);
      if (!done) done = luaPushAsWrapper<azApplication>(L, v);

      if (!done && boost::any_cast<wxString>(&v)) {
        lua_pushstring(L, boost::any_cast<wxString>(&v)->mb_str());
        done = true;
      }

      try {
        if (!done && boost::any_cast<int>(v)) {
          lua_pushinteger(L, boost::any_cast<int>(v));
          done = true;
        }
      } catch(boost::bad_any_cast &e) {}

      if (!done) wxLogDebug(_T("[ERROR] Cannot detect type of ") + k);

      lua_settable(L, tableIndex);      
    }

    lua_pushstring(L, "sender");

    bool done = false;

    done = luaConvertAndPushAsWrapper<FolderItemRenderer, azIcon>(L, sender);
    if (!done) done = luaConvertAndPushAsWrapper<wxMenu, azMenu>(L, sender);
    if (!done) done = luaConvertAndPushAsWrapper<OptionButton, azOptionButton>(L, sender);
    //if (!done) done = luaConvertAndPushAsWrapper<FolderItem, azShortcut>(L, sender);

    if (!done && dynamic_cast<MiniLaunchBar*>(sender)) {
      Lunar<azApplication>::push(L, wxGetApp().GetPluginManager()->luaApplication, true);
      done = true;
    }

    if (!done && dynamic_cast<OptionPanel*>(sender)) {
      Lunar<azOptionPanel>::push(L, wxGetApp().GetPluginManager()->luaOptionPanel, true);
      done = true;
    }

    if (!done) wxLogDebug(_T("[ERROR] Cannot detect type of sender"));

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