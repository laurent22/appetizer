/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginManager_H
#define __PluginManager_H


#include "Plugin.h"
#include "lua_glue/azApplication.h"


class PluginManager {

public:

  PluginManager();
  ~PluginManager();

  void Initialize();  
  PluginSP GetPluginByLuaState(lua_State* L);
  void DispatchEvent(wxObject* senderOrGlobalHook, int eventId, LuaHostTable arguments, wxObject* sender = NULL);
  void DispatchEvent(wxObject* senderOrGlobalHook, const wxString& eventName, LuaHostTable arguments, wxObject* sender = NULL);
  bool HandleMenuItemClick(ExtendedMenuItem* menuItem);
  int GetEventIdByName(const wxString& eventName);

  azApplication* luaApplication;

private:

  std::vector<PluginSP> plugins_;
  wxSortedArrayString  eventNames_;

};


#endif // __Plugin_H