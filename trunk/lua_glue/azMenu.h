/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azMenu_H
#define __azMenu_H


class azMenu : public wxObject {

public:

  azMenu(wxMenu* menu) { menu_ = menu; }
  azMenu(lua_State *L);

  int appendSeparator(lua_State *L);
  int append(lua_State *L);
  int appendSubMenu(lua_State *L);
  
  static const char className[];
  static Lunar<azMenu>::RegType methods[];

  wxMenu* menu_;  

};


#endif // __azMenu_H