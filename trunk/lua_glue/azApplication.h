/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azApplication_H
#define __azApplication_H


class azApplication : public wxObject {

public:

  azApplication() {}
  azApplication(lua_State *L) {}

  int addEventListener(lua_State *L);
  int getShortcutRoot(lua_State *L);
  int getShortcutById(lua_State *L);
  
  static const char className[];
  static Lunar<azApplication>::RegType methods[];

};


#endif // __azApplication_H