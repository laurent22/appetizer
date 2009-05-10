/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azPlugin_H
#define __azPlugin_H


#include "azWrapper.h"
#include "../Plugin.h"



class azPlugin : public azWrapper {

public:

  azPlugin(lua_State *L);
  ~azPlugin();

  int getPath(lua_State *L);  
  
  static const char className[];
  static Lunar<azPlugin>::RegType methods[];

  Plugin* Get() const;

private:


};


#endif // __azPlugin_H