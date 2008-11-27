/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __LuaUtil_H
#define __LuaUtil_H


typedef std::map<wxString, wxString> LuaHostTable;


class LuaUtil {

public:

  static wxString GetErrorString(int luaError);
  static void LogError(int luaError);
  static wxString ToString(lua_State *L, int n);
  static wxString GetStringFromTable(lua_State *L, int tableIndex, const wxString& key);

};


#endif // __LuaUtil_H