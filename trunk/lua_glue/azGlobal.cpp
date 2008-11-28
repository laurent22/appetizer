/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


#include "../stdafx.h"

#include "azGlobal.h"


void luaHost_logError(const wxString& s, const wxString& functionName) {
  wxLogDebug(_T("%s %s (%s)"), _T("[Lua Host] [Error]"), s, functionName);
}


int azPrint(lua_State *L) {
  int argc = lua_gettop(L);

  wxString output;

  for (int n = 1; n <= argc; ++n) {
    const char* c = lua_tostring(L, n);
    if (!c) {
      output += _T("<null>");
      continue;
    }

    output += wxString::FromUTF8(c);
  }

  wxLogDebug(_T("%s %s"), _T("[Lua Script]"), output);

  return 0;
}