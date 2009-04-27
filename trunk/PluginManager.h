/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __PluginManager_H
#define __PluginManager_H


#include "Plugin.h"
#include "lua_glue/azWrapper.h"

class azApplication;
#include "lua_glue/azApplication.h"
#include "lua_glue/azOptionPanel.h"
class azDialogs;
#include "lua_glue/azDialogs.h"
class azSystem;
#include "lua_glue/azSystem.h"


class PluginManager {

public:

  PluginManager();
  ~PluginManager();

  void Initialize();  
  Plugin* GetPluginByLuaState(lua_State* L);
  void DispatchEvent(wxObject* sender, int eventId, LuaHostTable& arguments);
  void DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable& arguments);
  bool HandleMenuItemClick(ExtendedMenuItem* menuItem);
  bool InstallPluginPackage(const wxString& filePath);
  int GetEventIdByName(const wxString& eventName);
  PluginVector GetPlugins();
  void Save();

  azApplication* luaApplication;
  azOptionPanel* luaOptionPanel;
  azDialogs* luaDialogs;
  azSystem* luaSystem;

private:

  PluginVector plugins_;
  wxSortedArrayString  eventNames_;
  bool initialized_;

};


#endif // __PluginManager_H