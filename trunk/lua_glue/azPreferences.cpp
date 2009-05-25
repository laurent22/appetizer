/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * Preferences provide a persitent data storage for plugins. The preferences are saved
 * when Appetizer is closed and restored when it restarts. Preferences must be defined
 * at the begining of a Lua script using <code>registerPreference()</code>. And it is
 * then possible to get or set their values using <code>getValue()</code> and <code>setValue()</code>.
 * If a plugin has preferences the "Options" button will be enabled in the Configuration dialog, 
 * which will allow the user to change the preferences. It is also possible to programmatically
 * show this dialog box using <code>dialogs:showPreferences()</code>
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
  method(azPreferences, registerPreferenceGroup),
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


azPreferences::azPreferences(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Instead use the global 'preferences' object.");
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * Preferences can be grouped into tabs. To create a new group, use this function. you
 * can then assign a preference to it by using the <code>group</code> property.
 * @example This script registers a preference group.
 * <listing version="3.0">
 * preferences:registerPreferenceGroup("advancedOptions", "Advanced")
 * </listing>
 * @param String name Name of the group
 * @param String title Title of the group
 *
 */	
int azPreferences::registerPreferenceGroup(lua_State *L) {
  wxString inputName = LuaUtil::GetStringFromTable(L, 1, _T("name"), false);
  wxString inputTitle = LuaUtil::GetStringFromTable(L, 1, _T("title"));

  PluginPreferenceGroup* group = new PluginPreferenceGroup();
  group->Name = inputName;
  group->Title = inputTitle;

  Get()->RegisterPreferenceGroup(group);

  return 0;
}


/**
 * Registers a preference. Preferences must be registered before they appear in the Preference
 * dialog, and before their values can be accessed. A preference is an associative array, which
 * can contain the following values:
 * <table class=innertable>
 * <tr><th>Name</th><th>Type</th><th>Required?</th><th>Description</th></tr>  
 * <tr><td><code>type</code></td><td>String</td><td>yes</td><td>Can be any of these types: Text, TextArea, Popup, CheckBox, File, Spinner, Hidden</td></tr>
 * <tr><td><code>name</code></td><td>String</td><td>yes</td><td>Name of the preference. Must be unique.</td></tr>
 * <tr><td><code>defaultValue</code></td><td>Any</td><td>no</td><td>Default value.</td></tr>
 * <tr><td><code>title</code></td><td>String</td><td>no</td><td>Title of the preference, as displayed in the Preference dialog.</td></tr>
 * <tr><td><code>description</code></td><td>String</td><td>no</td><td>Description of the preference. It is displayed below the control in the Preference dialog</td></tr>
 * <tr><td><code>group</code></td><td>String</td><td>no</td><td>Optional preference group</td></tr>
 * <tr><td><code>minValue</code></td><td>Number</td><td>no</td><td>Used by the Spinner control to specify the value range</td></tr>
 * <tr><td><code>maxValue</code></td><td>Number</td><td>no</td><td>Used by the Spinner control to specify the value range</td></tr>
 * <tr><td><code>secure</code></td><td>Boolean</td><td>no</td><td>Sets it to true to make a text field secure (eg. for password input)</td></tr>
 * <tr><td><code>options</code></td><td>Array</td><td>no</td><td>Associative array of key / value pairs to specify the available options for the Popup control.</td></tr>
 * </table>  
 * @example This script registers two preferences.
 * <listing version="3.0">
 * -- The example below register two preferences. One text area and one checkbox
 *
 * preferences:registerPreference({
 *   type = "TextArea",
 *   name = "textAreaExample",
 *   defaultValue = "Type-in some text in this boxx",
 *   title = "Text area example:"
 * })
 *
 * preferences:registerPreference({
 *   type = "CheckBox",
 *   name = "checkboxExample",
 *   defaultValue = false,
 *   title = "Are you sure?"
 * })
 * </listing>
 * @param Array preference Preference to register
 *
 */	
int azPreferences::registerPreference(lua_State *L) {
  PluginPreference* preference = LuaUtil::ToPluginPreference(L, Get(), 1);
  
  Get()->RegisterPreference(preference);

  return 0;
}


/**
 * Gets the value of the given preference.
 * @param String name Name of the preference
 * @return Object Value of the preference
 *
 */	
int azPreferences::getValue(lua_State *L) {
  wxString inputName = LuaUtil::ToString(L, 1, false); 
  
  PluginPreference* preference = preferences_->GetPreference(inputName);
  if (!preference) return 0;

  if (preference->GetType() == PluginPreferenceType::Spinner) {
    lua_pushinteger(L, preference->GetIntValue());
    return 1;
  }

  if (preference->GetType() == PluginPreferenceType::CheckBox) {
    lua_pushboolean(L, preference->GetBoolValue());
    return 1;
  }

  lua_pushstring(L, preference->GetValue().ToUTF8());

  return 1;
}


/**
 * Sets the value of the given preference.
 * @param String name Name of the preference
 * @param Object value New value of the preference
 *
 */	
int azPreferences::setValue(lua_State *L) {
  wxString inputName = LuaUtil::ToString(L, 1, false);
  wxString inputValue = LuaUtil::ToString(L, 2, false); 
  
  PluginPreference* preference = preferences_->GetPreference(inputName);
  if (!preference) return 0;

  preference->SetValue(inputValue);

  preferences_->ScheduleSave();

  return 0;
}