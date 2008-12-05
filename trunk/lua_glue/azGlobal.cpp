/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


#include "../stdafx.h"

#include "azGlobal.h"


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


int luaHost_logError(const wxString& s) {
  ELOG(_T("%s %s"), _T("[Lua Host] [Error]"), s);
  return 0;
}


int luaHost_logInfo(const wxString& s) {
  ILOG(_T("%s %s"), _T("[Lua Host] [Info]"), s);
  return 0;
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


int azPrint(lua_State *L) {
  int argc = lua_gettop(L);

  wxString output;

  for (int n = 1; n <= argc; ++n) {
    if (lua_isboolean(L, n)) {
      output += lua_toboolean(L, n) ? _T("true") : _T("false");
      continue;
    }

    const char* c = lua_tostring(L, n);
    if (!c) {
      output += _T("<null>");
      continue;
    }

    output += wxString::FromUTF8(c);
  }

  ILOG(_T("%s %s"), _T("[Lua Script]"), output);

  return 0;
}