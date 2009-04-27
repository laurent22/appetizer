/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __Plugin_H
#define __Plugin_H


#include "TypeDefinitions.h"
#include "ExtendedMenuItem.h"
#include "lua_glue/LuaUtil.h"
#include "lua_glue/azPreferences.h"


class Plugin {

public:

  Plugin();
  ~Plugin();

  void LoadMetadata(const wxString& folderPath);
  bool Load(const wxString& folderPath);
  void AddEventListener(wxObject* object, int eventId, const wxString& functionName);
  void AddEventListener(wxObject* object, const wxString& eventName, const wxString& functionName);
  void DispatchEvent(wxObject* sender, int eventId, LuaHostTable& arguments);
  void DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable& arguments);
  lua_State* GetLuaState();
  wxString GetName();
  wxString GetUUID();
  void OnLuaScopeClose();
  bool WasInitiallyEnabled();
  void SetInitiallyEnabled(bool enabled);
  void Enable(bool enable);
  void Disable();
  bool IsEnabled();
  wxString GetState();
  bool HandleMenuItemClick(ExtendedMenuItem* menuItem);

private:

  lua_State* L;

  bool initiallyEnabled_;
  bool enabled_;
  wxString uuid_;
  wxString name_;
  wxString state_;

  PluginPreferences* preferences_;
  azPreferences* luaPreferences_;

  void LoadPluginXml(const wxString& xmlFilePath);

  std::map<std::pair<wxObject*, int>, wxArrayString*> eventRegister_;

};


typedef std::vector<Plugin*> PluginVector;


#endif // __Plugin_H