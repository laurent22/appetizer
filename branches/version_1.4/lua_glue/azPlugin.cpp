/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * Represents a plugin object. You do not need to directly create an instance
 * of it. Instead, directly use the methods and properties of the <code>plugin</code> global object.
 *  
 * @beginEventTable
 * @event PreferenceChange preferenceChange when the preferences of the plugin have been changed
 * @endEventTable
 * @see Global#plugin
 */	


#include "../stdafx.h"

#include "azPlugin.h"
#include "../MiniLaunchBar.h"



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azPlugin::className[] = "Plugin";

#define method(class, name) {#name, &class::name}

Lunar<azPlugin>::RegType azPlugin::methods[] = {
  method(azPlugin, getDirectory),
  method(azPlugin, addEventListener),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azPlugin::azPlugin(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Instead use the global 'plugin' object.");
}


azPlugin::azPlugin() {}


azPlugin::~azPlugin() {

}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * Gets the plugin directory.
 * @return String The plugin directory.
 * 
 */	
int azPlugin::getDirectory(lua_State *L) {
  Plugin* plugin = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  if (!plugin) LuaNullError(L);

  LuaUtil::PushString(L, plugin->GetFolderPath());

  return 1;
}


/**
 * @copy Application#addEventListener()
 * 
 */	
int azPlugin::addEventListener(lua_State *L) {
  wxString eventName = LuaUtil::ToString(L, 1);
  wxString functionName = LuaUtil::ToString(L, 2);
  Plugin* p = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  p->AddEventListener(p, eventName, functionName);
  return 0;
}
