/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginManager_H
#define __PluginManager_H


#include "Plugin.h"


class PluginManager {

public:

  PluginManager();
  
  PluginSP GetPluginByLuaState(lua_State* L);
  void DispatchEvent(void* senderOrGlobalHook, int eventId, LuaHostTable arguments, void* sender = NULL);
  bool HandleMenuItemClick(ExtendedMenuItem* menuItem);

private:

  std::vector<PluginSP> plugins_;

};


#endif // __Plugin_H