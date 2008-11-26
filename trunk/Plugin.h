/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __Plugin_H
#define __Plugin_H


#include "TypeDefinitions.h"
#include "utilities/LuaUtil.h"


class Plugin {

public:

  Plugin();
  ~Plugin();

  void LoadFile(const wxString& luaFilePath);
  void AddEventListener(int objectId, int eventId, const wxString& functionName);
  void DispatchEvent(int objectId, int eventId, LuaHostTable arguments);
  lua_State* GetLuaState();

private:

  lua_State* L;
  std::map<std::pair<int, int>, wxArrayString*> eventRegister_;

};


typedef boost::shared_ptr<Plugin> PluginSP;


#endif // __Plugin_H