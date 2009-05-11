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
  method(azPlugin, getPath),
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


int azPlugin::getPath(lua_State *L) {
  Plugin* plugin = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  if (!plugin) LuaNullError(L);

  LuaUtil::PushString(L, plugin->GetFolderPath());

  return 1;
}
