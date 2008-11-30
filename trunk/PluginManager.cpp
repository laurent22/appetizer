/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginManager.h"
#include "FilePaths.h"
#include "FolderItem.h"
#include "OptionButton.h"
#include "FolderItemRenderer.h"
#include "lua_glue/LuaUtil.h"
#include "lua_glue/azOptionButton.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azShortcut.h"


PluginManager::PluginManager() {
  luaApplication = NULL;
}


PluginManager::~PluginManager() {
  for (int i = 0; i < plugins_.size(); i++) wxDELETE(plugins_[i]);

  wxDELETE(luaApplication);
  wxDELETE(luaOptionPanel);
}


void PluginManager::Initialize() {
  eventNames_.Add(_T("iconMenuOpening"));
  eventNames_.Add(_T("click"));

  luaApplication = new azApplication();
  luaOptionPanel = new azOptionPanel();

  wxString pluginPath = FilePaths::GetPluginsDirectory();
  wxDir pluginFolder;

  if (wxFileName::DirExists(pluginPath) && pluginFolder.Open(pluginPath)) {
    wxString folderName;
    bool success = pluginFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    
    while (success) {
      wxLogDebug(_T("Loading plugin: ") + folderName);

      Plugin* p  = new Plugin();
      plugins_.push_back(p);

      p->LoadFile(pluginPath + wxFileName::GetPathSeparator() + folderName + wxFileName::GetPathSeparator() + _T("main.lua"));

      success = pluginFolder.GetNext(&folderName);
    }
  }
}


int PluginManager::GetEventIdByName(const wxString& eventName) {
  return eventNames_.Index(eventName);
}


Plugin* PluginManager::GetPluginByLuaState(lua_State* L) {
  for (int i = 0; i < plugins_.size(); i++) {
    if (plugins_.at(i)->GetLuaState() == L) return plugins_.at(i);
  }

  return NULL;
}


void PluginManager::DispatchEvent(wxObject* sender, int eventId, LuaHostTable arguments) {
  for (int i = 0; i < plugins_.size(); i++) {
    plugins_.at(i)->DispatchEvent(sender, eventId, arguments);
  }

  LuaUtil::DestroyLuaHostTable(&arguments);
}


void PluginManager::DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable arguments) {
  int eventId = GetEventIdByName(eventName);
  DispatchEvent(sender, eventId, arguments);
}


bool PluginManager::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  lua_State* luaState = (lua_State*)menuItem->GetMetadataPointer(_T("plugin_luaState"));
  if (!luaState) return false;

  Plugin* p = GetPluginByLuaState(luaState);
  if (!p) return false;

  return p->HandleMenuItemClick(menuItem);
}