/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azApplication_H
#define __azApplication_H


#include "azGlobal.h"
#include "azWrapper.h"


class MiniLaunchBar;
#include "../MiniLaunchBar.h"


class azApplication : public azWrapper {

public:

  azApplication() {}
  azApplication(lua_State *L) {}

  int getShortcutRoot(lua_State *L);
  int getShortcutById(lua_State *L);

  azDECLARE_EVENT_LISTENER_FUNCTION()
  
  static const char className[];
  static Lunar<azApplication>::RegType methods[];

  MiniLaunchBar* Get() const;

};


#endif // __azApplication_H