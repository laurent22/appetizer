/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "LuaUtil.h"

#include "azWrapper.h"
#include "azGlobal.h"
#include "azApplication.h"
//#include "azIcon.h"
#include "azOptionPanel.h"
#include "azOptionButton.h"
#include "azMenu.h"
#include "azDockItem.h"
#include "../FolderItemRenderer.h"
#include "../OptionButton.h"



LuaHostTableItem::LuaHostTableItem(wxObject* value, LuaHostTableItemType valueType) {
  this->value = value;
  this->valueType = valueType;
}


LuaHostTable::~LuaHostTable() {
  for (LuaHostTable::iterator i = this->begin(); i != this->end(); ++i) {
    LuaHostTableItem* item = i->second;    
    wxDELETE(item);
  }
}


template <class hostObjectT, class lunarObjectT>
bool luaConvertAndPushAsWrapper(lua_State* L, wxObject* o) {
  hostObjectT* asType = dynamic_cast<hostObjectT*>(o);
  if (asType) {
    Lunar<lunarObjectT>::push(L, new lunarObjectT(asType), true);
    return true;
  }
  return false;
}


bool LuaUtil::DetectTypeAndPushAsWrapper(lua_State* L, wxObject* value) {
  // wxObject* must be any supported wxWidgets object EXCEPT for azWrappers
  // The function checks the type of "value" and tries to find an associated wrapper.
  // If it does, the object is converted to an azWrapper and pushed onto the Lua stack.
  // If it doesn't, the function returns false.

  bool done = false;

  done = luaConvertAndPushAsWrapper<wxMenu, azMenu>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<FolderItem, azDockItem>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<OptionButton, azOptionButton>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<FolderItem, azDockItem>(L, value);
  if (done) return true;

  if (dynamic_cast<MiniLaunchBar*>(value)) {
    Lunar<azApplication>::push(L, wxGetApp().GetPluginManager()->luaApplication);
    return true;
  }

  if (dynamic_cast<OptionPanel*>(value)) {
    Lunar<azOptionPanel>::push(L, wxGetApp().GetPluginManager()->luaOptionPanel);
    return true;
  }

  return false;
}


int LuaUtil::ConvertAndPushLuaHostTable(lua_State *L, LuaHostTable& table) {
  lua_createtable(L, table.size(), 0);
  int tableIndex = lua_gettop(L);
  
  LuaHostTable::iterator it = table.begin();

  for(; it != table.end(); ++it) {
    wxString k = it->first;
    LuaHostTableItem* hostTableItem = it->second;

    wxObject* value = hostTableItem->value;
    LuaHostTableItemType valueType = hostTableItem->valueType;

    lua_pushstring(L, k.mb_str());

    bool done = true;

    if (valueType == LHT_boolean) {

      lua_pushboolean(L, *((wxString*)value) != _T("0"));

    } else if (valueType == LHT_integer) {

      wxString* s = (wxString*)value;
      long l; if (!s->ToLong(&l)) l = 0;
      lua_pushinteger(L, (int)l);

    } else if (valueType == LHT_string) {

      wxString* s = (wxString*)value;
      lua_pushstring(L, s->ToUTF8());

    } else if (valueType == LHT_wxObject) {

      done = DetectTypeAndPushAsWrapper(L, value);

    } else {

      done = false;

    }

    if (!done) wxLogDebug(_T("[ERROR] LuaUtil::ConvertAndPushLuaHostTable cannot detect type of ") + k);

    lua_settable(L, tableIndex);      
  }

  return tableIndex;
}


wxString LuaUtil::GetErrorString(int luaError) {
  switch (luaError) {
    case 0: return _T(""); break;
    case LUA_ERRSYNTAX: return _T("Syntax error"); break;
    case LUA_ERRMEM: return _T("Memory allocation error"); break;
    case LUA_ERRFILE: return _T("Cannot open / read file"); break;
    case LUA_ERRRUN: return _T("Runtime error"); break;
    case LUA_ERRERR: return _T("Error while running the error handler function"); break;
  }

  return wxString::Format(_T("%s (%d)"), _T("Unknown error"), luaError);
}


void LuaUtil::LogError(int luaError) {
  wxLogDebug(_T("[Lua Error %d] %s"), luaError, LuaUtil::GetErrorString(luaError));
}


void LuaUtil::PushString(lua_State *L, const wxString& s) {
  lua_pushstring(L, s.ToUTF8());
}


wxString LuaUtil::ToString(lua_State *L, int n, bool isOptional, const wxString& defaultValue) {
  if (isOptional) {
    if (!lua_isstring(L, n)) return defaultValue;
  }
  
  wxString output(luaL_checkstring(L, n), wxConvUTF8);
  return output;
}


bool LuaUtil::ToBoolean(lua_State *L, int n, bool isOptional, bool defaultValue) {
  if (isOptional) {
    if (!lua_isboolean(L, n)) {
      return defaultValue;
    } else {
      return lua_toboolean(L, n);
    }
  } else {
    if (!lua_isboolean(L, n)) {
      luaL_typerror(L, n, lua_typename(L, LUA_TBOOLEAN));
      return false;
    }
    return lua_toboolean(L, n);
  }
}


wxString LuaUtil::GetStringFromTable(lua_State *L, int tableIndex, const wxString& key, bool isOptional) {
  lua_pushstring(L, key.mb_str());
  lua_gettable(L, tableIndex);
  
  wxString output;

  if (lua_isboolean(L, -1)) {
    bool bOutput = LuaUtil::ToBoolean(L, -1, isOptional);
    return bOutput ? _T("true") : _T("false");
  }

  if (!lua_isstring(L, -1)) {
    if (!isOptional) {
      luaL_typerror(L, -1, lua_typename(L, LUA_TSTRING));
    }
  } else {
    output = LuaUtil::ToString(L, -1);
  }
  
  lua_pop(L, 1);

  return output;
}


bool LuaUtil::GetBoolFromTable(lua_State *L, int tableIndex, const wxString& key, bool isOptional) {
  lua_pushstring(L, key.mb_str());
  lua_gettable(L, tableIndex);
  
  bool output = false;

  if (!lua_isboolean(L, -1)) {
    if (!isOptional) {
      luaL_typerror(L, -1, lua_typename(L, LUA_TSTRING));
    }
  } else {
    output = LuaUtil::ToBoolean(L, -1);
  }
  
  lua_pop(L, 1);

  return output;
}


PluginPreference* LuaUtil::ToPluginPreference(lua_State *L, PluginPreferences* preferences, int index) {
  wxString inputType = LuaUtil::GetStringFromTable(L, index, _T("type"), false);
  wxString inputName = LuaUtil::GetStringFromTable(L, index, _T("name"), false);
  wxString inputDefaultValue = LuaUtil::GetStringFromTable(L, index, _T("defaultValue"));
  wxString inputTitle = LuaUtil::GetStringFromTable(L, index, _T("title"));
  wxString inputDescription = LuaUtil::GetStringFromTable(L, index, _T("description"));
  wxString inputGroup = LuaUtil::GetStringFromTable(L, index, _T("group"));
  wxString inputMinValue = LuaUtil::GetStringFromTable(L, index, _T("minValue"));
  wxString inputMaxValue = LuaUtil::GetStringFromTable(L, index, _T("maxValue"));
  bool inputSecure = LuaUtil::GetBoolFromTable(L, index, _T("secure"));

  PluginPreferenceOptions inputOptions;

  lua_pushstring(L, "options");
  lua_gettable(L, index);
  int isTable = lua_istable(L, -1);

  if (isTable) {
    lua_pushnil(L);

    while (lua_next(L, -2) != 0) {
      lua_pushvalue(L, -2); // Push a copy of the key onto the stack
      const char* key = lua_tostring(L, -1); // Get the key
      lua_pop(L, 1); // Pop the key copy off

      lua_pushvalue(L, -1); // Push a copy of the value onto the stack
      const char* value = lua_tostring(L, -1); // Get the value
      lua_pop(L, 1); // Pop the value copy off

      inputOptions[wxString(key, wxConvUTF8)] = wxString(value, wxConvUTF8);

      lua_pop(L, 1); // Pop the value off
    }
    
  }

  lua_pop(L, 1); // Pop the "options" table off


  int prefType = 0;

  if (preferences->GetPreference(inputName)) {
    luaL_error(L, wxString::Format(_T("There is already a preference with this name: %s"), inputName).mb_str());
    return NULL;
  }

  if (inputType == _T("Text")) {
    prefType = PluginPreferenceType::Text;
  } else if (inputType == _T("TextArea")) {
    prefType = PluginPreferenceType::TextArea;
  } else if (inputType == _T("Popup")) {
    prefType = PluginPreferenceType::Popup;
  } else if (inputType == _T("CheckBox")) {
    prefType = PluginPreferenceType::CheckBox;
  } else if (inputType == _T("File")) {
    prefType = PluginPreferenceType::File;
  } else if (inputType == _T("Spinner")) {
    prefType = PluginPreferenceType::Spinner;
  } else if (inputType == _T("Hidden")) {
    prefType = PluginPreferenceType::Hidden;
  } else {
    luaL_error(L, wxString::Format(_T("Unknown preference type: %s"), inputType).mb_str());
    return NULL;
  }

  PluginPreferenceGroup* group = NULL;
  
  if (inputGroup != wxEmptyString) {
    group = preferences->GetPreferenceGroup(inputGroup);
    if (!group) {
      group = new PluginPreferenceGroup();
      group->Name = inputGroup;
      group->Title = inputGroup;
      preferences->RegisterPreferenceGroup(group);
    }
  }

  PluginPreference* preference = new PluginPreference(prefType, inputName, inputDefaultValue, inputTitle, inputDescription, group, inputOptions);
  if (inputMinValue != wxEmptyString && inputMaxValue != wxEmptyString) {
    double dMin = 0;
    bool success = inputMinValue.ToDouble(&dMin);
    if (!success) {
      luaL_error(L, wxString::Format(_T("%s in not a number"), inputMinValue).mb_str());
      return NULL;
    }

    double dMax = 0;
    success = inputMaxValue.ToDouble(&dMax);
    if (!success) {
      luaL_error(L, wxString::Format(_T("%s in not a number"), inputMaxValue).mb_str());
      return NULL;
    }

    preference->SetRange(dMin, dMax);
  }

  preference->SetSecure(inputSecure);

  return preference;
}