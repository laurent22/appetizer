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
  azApplication(lua_State *L);

  int addEventListener(lua_State *L);
  int getDockItemsRoot(lua_State *L);
  int getDockItemById(lua_State *L);
  int hide(lua_State *L);
  int show(lua_State *L);
  int close(lua_State *L);
  int isVisible(lua_State *L);
  int setOrientation(lua_State *L);
  int getOrientation(lua_State *L);
  int doMultiLaunch(lua_State *L);
  int installAutoRunFile(lua_State *L);
  int getDrive(lua_State *L);
  int showHelpFile(lua_State *L);
  int getSkinNames(lua_State *L);
  int setSkin(lua_State *L);
  int enable(lua_State *L);
  int disable(lua_State *L);

  static const char className[];
  static Lunar<azApplication>::RegType methods[];

  MiniLaunchBar* Get() const;

};


#endif // __azApplication_H