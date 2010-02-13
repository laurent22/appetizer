/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azGlobal_H
#define __azGlobal_H

int luaHost_logError(const wxString& s);
int luaHost_logInfo(const wxString& s);

int azPrint(lua_State *L);
int azTranslate(lua_State *L);
int azCancelEvent(lua_State *L);


#endif // __azGlobal_H