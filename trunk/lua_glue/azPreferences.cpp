/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * TODO
 *
 */	


#include "../stdafx.h"

#include "azPreferences.h"
#include "LuaUtil.h"


//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azPreferences::className[] = "Preferences";

#define method(class, name) {#name, &class::name}

Lunar<azPreferences>::RegType azPreferences::methods[] = {
  method(azPreferences, registerPreference),
  method(azPreferences, getValue),
  method(azPreferences, setValue),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azPreferences::azPreferences(PluginPreferences* preferences) {
  preferences_ = preferences;
}


PluginPreferences* azPreferences::Get() const {
  return preferences_;
}


azPreferences::~azPreferences() {

}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azPreferences::azPreferences(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Instead use the global 'preferences' object.");
}


int azPreferences::registerPreference(lua_State *L) {
  wxString inputType = LuaUtil::GetStringFromTable(L, 1, _T("type"), false);
  wxString inputName = LuaUtil::GetStringFromTable(L, 1, _T("name"), false);
  wxString inputDefaultValue = LuaUtil::GetStringFromTable(L, 1, _T("defaultValue"));
  wxString inputTitle = LuaUtil::GetStringFromTable(L, 1, _T("title"));
  wxString inputDescription = LuaUtil::GetStringFromTable(L, 1, _T("description"));

  PluginPreferenceOptions inputOptions;

  lua_pushstring(L, "options");
  lua_gettable(L, 1);
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


  int prefType = 0;

  if (Get()->GetPreference(inputName)) {
    luaL_error(L, wxString::Format(_T("There is already a preference with this name: %s"), inputName).mb_str());
    return 0;
  }

  if (inputType == _T("Text")) {
    prefType = PluginPreferenceType::Text;
  } else if (inputType == _T("TextArea")) {
    prefType = PluginPreferenceType::TextArea;
  } else if (inputType == _T("Popup")) {
    prefType = PluginPreferenceType::Popup;
  } else {
    luaL_error(L, wxString::Format(_T("Unknown preference type: %s"), inputType).mb_str());
    return 0;
  }

  PluginPreference* preference = new PluginPreference(prefType, inputName, inputDefaultValue, inputTitle, inputDescription, inputOptions);
  Get()->RegisterPreference(preference);

  return 0;
}



int azPreferences::getValue(lua_State *L) {
  wxString inputName = LuaUtil::ToString(L, 1, false); 
  
  PluginPreference* preference = preferences_->GetPreference(inputName);
  if (!preference) return 0;

  lua_pushstring(L, preference->GetValue().ToUTF8());

  return 1;
}


int azPreferences::setValue(lua_State *L) {
  wxString inputName = LuaUtil::ToString(L, 1, false);
  wxString inputValue = LuaUtil::ToString(L, 2, false); 
  
  PluginPreference* preference = preferences_->GetPreference(inputName);
  if (!preference) return 0;

  preference->SetValue(inputValue);

  preferences_->Save();

  return 0;
}


