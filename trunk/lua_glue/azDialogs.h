/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azDialogs_H
#define __azDialogs_H


#include "azWrapper.h"


class azDialogs : public azWrapper {

public:

  azDialogs();
  azDialogs(lua_State *L);

  int showMessage(lua_State *L);
  int showConfigDialog(lua_State *L);
  int showNewShortcutDialog(lua_State *L);
  int showImportDialog(lua_State *L);
  int showEjectDriveDialog(lua_State *L); 
  int showPreferences(lua_State *L);
  int showForm(lua_State *L);
  
  static const char className[];
  static Lunar<azDialogs>::RegType methods[];
  
};


#endif // __azDialogs_H