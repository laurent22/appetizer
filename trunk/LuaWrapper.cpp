/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


#include "stdafx.h"

#include "LuaWrapper.h"
#include "utilities/LuaUtil.h"
#include "Plugin.h"
#include "Log.h"
#include "MiniLaunchBar.h"


int az_print(lua_State *L) {
  int argc = lua_gettop(L);

  wxString output;

  for (int n = 1; n <= argc; ++n) {
    const char* c = lua_tostring(L, n);
    if (!c) {
      output += _T("<null>");
      continue;
    }

    int cLen = strlen(c);
    wxString s;
    for (int j = 0; j < cLen; j++) {
      s += wxChar(c[j]);
    }
    output += s;
  }

  wxLogDebug(_T("%s %s"), _T("[Lua]"), output);

  return 0;
}


int az_addEventListener(lua_State *L) {
  int objectId = lua_tointeger(L, 1);
  int eventId = lua_tointeger(L, 2);
  wxString functionName = LuaUtil::ToString(L, 3);
 
  PluginSP p = wxGetApp().GetPluginManager().GetPluginByLuaState(L);
  
  if (!p.get()) {
    elog("Could not find matching plugin for Lua state");
    return 0;
  }

  p->AddEventListener(objectId, eventId, functionName);

  return 0;
}
