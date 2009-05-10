/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"
#include "../PluginPreference.h"
#include "../PluginPreferences.h"


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


class LuaHostTable : public std::map<wxString, LuaHostTableItem*> {

public:

  LuaHostTable() {}
  ~LuaHostTable();

};


class LuaUtil {

public:

  static wxString GetErrorString(int luaError);
  static void LogError(int luaError);
  static void PushString(lua_State *L, const wxString& s);
  static wxString ToString(lua_State *L, int n, bool isOptional = false, const wxString& defaultValue = wxEmptyString);
  static wxString GetStringFromTable(lua_State *L, int tableIndex, const wxString& key, bool isOptional = true);
  static bool GetBoolFromTable(lua_State *L, int tableIndex, const wxString& key, bool isOptional = true);
  static bool ToBoolean(lua_State* L, int n, bool isOptional = false, bool defaultValue = false);
  static PluginPreference* ToPluginPreference(lua_State *L, PluginPreferences* preferences, int index);

  static bool DetectTypeAndPushAsWrapper(lua_State* L, wxObject* value);
  static int ConvertAndPushLuaHostTable(lua_State *L, LuaHostTable& table);

};


#endif // __LuaUtil_H