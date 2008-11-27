/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginManager.h"
#include "LuaWrapper.h"
#include "utilities/LuaUtil.h"


PluginManager::PluginManager() {
  PluginSP p(new Plugin());
  plugins_.push_back(p);

  p->LoadFile(_T("Data\\Plugins\\AddToGroupOnRightClick.lua"));
}


PluginSP PluginManager::GetPluginByLuaState(lua_State* L) {
  for (int i = 0; i < plugins_.size(); i++) {
    if (plugins_.at(i)->GetLuaState() == L) return plugins_.at(i);
  }

  PluginSP nullOutput;
  return nullOutput;
}


void PluginManager::DispatchEvent(void* senderOrGlobalHook, int eventId, LuaHostTable arguments, void* sender) {
  for (int i = 0; i < plugins_.size(); i++) {
    plugins_.at(i)->DispatchEvent(senderOrGlobalHook, eventId, arguments, sender);
  }
}


bool PluginManager::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  lua_State* luaState = (lua_State*)menuItem->GetMetadataPointer(_T("plugin_luaState"));
  if (!luaState) return false;

  PluginSP p = GetPluginByLuaState(luaState);
  if (!p.get()) return false;

  return p->HandleMenuItemClick(menuItem);
}