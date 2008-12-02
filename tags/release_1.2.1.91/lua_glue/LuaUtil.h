/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __LuaUtil_H
#define __LuaUtil_H


enum LuaHostTableItemType {
  LHT_boolean,
  LHT_integer,
  LHT_string,
  LHT_wxObject
};


class LuaHostTableItem {
  
public:

  LuaHostTableItem(wxObject* value, LuaHostTableItemType valueType);  
  wxObject* value;
  LuaHostTableItemType valueType;

};


typedef std::map<wxString, LuaHostTableItem*> LuaHostTable;


class LuaUtil {

public:

  static wxString GetErrorString(int luaError);
  static void LogError(int luaError);
  static void PushString(lua_State *L, const wxString& s);
  static wxString ToString(lua_State *L, int n);
  static wxString GetStringFromTable(lua_State *L, int tableIndex, const wxString& key, bool isOptional = true);
  static bool ToBoolean(lua_State* L, int n);
  static void DestroyLuaHostTable(LuaHostTable* t);

};


#endif // __LuaUtil_H