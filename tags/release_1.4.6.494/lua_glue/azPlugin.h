/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azPlugin_H
#define __azPlugin_H


#include "azWrapper.h"



class azPlugin : public azWrapper {

public:

  azPlugin();
  azPlugin(lua_State *L);
  ~azPlugin();

  int getDirectory(lua_State *L);  
  int addEventListener(lua_State *L);
  
  static const char className[];
  static Lunar<azPlugin>::RegType methods[];

};


#endif // __azPlugin_H