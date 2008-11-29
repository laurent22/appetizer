/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azGlobal_H
#define __azGlobal_H


#define azDECLARE_EVENT_LISTENER_FUNCTION() \
  int addEventListener(lua_State *L);

#define azIMPLEMENT_EVENT_LISTENER_FUNCTION(theClass) \
int theClass::addEventListener(lua_State *L) { \
  wxString eventName = LuaUtil::ToString(L, 1); \
  wxString functionName = LuaUtil::ToString(L, 2); \
  PluginSP p = wxGetApp().GetPluginManager()->GetPluginByLuaState(L); \
  p->AddEventListener(this->Get(), eventName, functionName); \
  return 0; \
}

int luaHost_logError(const wxString& s, const wxString& functionName);

int azPrint(lua_State *L);


#endif // __azGlobal_H