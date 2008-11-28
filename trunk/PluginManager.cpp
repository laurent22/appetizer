/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginManager.h"
#include "FilePaths.h"
#include "utilities/LuaUtil.h"


PluginManager::PluginManager() {
  luaApplication = NULL;
}


PluginManager::~PluginManager() {
  wxDELETE(luaApplication);
}


void PluginManager::Initialize() {
  eventNames_.Add(_T("iconMenuOpening"));

  luaApplication = new azApplication();

  wxString pluginPath = FilePaths::GetPluginsDirectory();
  wxDir pluginFolder;

  if (wxFileName::DirExists(pluginPath) && pluginFolder.Open(pluginPath)) {
    wxString folderName;
    bool success = pluginFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    
    while (success) {
      wxLogDebug(_T("Loading plugin: ") + folderName);

      PluginSP p(new Plugin());
      plugins_.push_back(p);

      p->LoadFile(pluginPath + wxFileName::GetPathSeparator() + folderName + wxFileName::GetPathSeparator() + _T("main.lua"));

      #ifdef __WXDEBUG__
      if (folderName == _T("TestUnit")) {
        lua_getfield(p->GetLuaState(), LUA_GLOBALSINDEX, "groupTest");        
        int errorCode = lua_pcall(p->GetLuaState(), 0, 0, 0);
        if (errorCode) {
          const char* errorString = lua_tostring(p->GetLuaState(), -1);
          wxLogDebug(_T("ERROR: ") + wxString(errorString, wxConvUTF8));
        }
      }
      #endif // __WXDEBUG__

      success = pluginFolder.GetNext(&folderName);
    }
  }
}


int PluginManager::GetEventIdByName(const wxString& eventName) {
  return eventNames_.Index(eventName);
}


PluginSP PluginManager::GetPluginByLuaState(lua_State* L) {
  for (int i = 0; i < plugins_.size(); i++) {
    if (plugins_.at(i)->GetLuaState() == L) return plugins_.at(i);
  }

  PluginSP nullOutput;
  return nullOutput;
}


void PluginManager::DispatchEvent(wxObject* senderOrGlobalHook, int eventId, LuaHostTable arguments, wxObject* sender) {
  for (int i = 0; i < plugins_.size(); i++) {
    plugins_.at(i)->DispatchEvent(senderOrGlobalHook, eventId, arguments, sender);
  }
}


void PluginManager::DispatchEvent(wxObject* senderOrGlobalHook, const wxString& eventName, LuaHostTable arguments, wxObject* sender) {
  int eventId = GetEventIdByName(eventName);
  DispatchEvent(senderOrGlobalHook, eventId, arguments, sender);
}


bool PluginManager::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  lua_State* luaState = (lua_State*)menuItem->GetMetadataPointer(_T("plugin_luaState"));
  if (!luaState) return false;

  PluginSP p = GetPluginByLuaState(luaState);
  if (!p.get()) return false;

  return p->HandleMenuItemClick(menuItem);
}