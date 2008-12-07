/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azMenuItem_H
#define __azMenuItem_H


#include "azWrapper.h"



struct azWrappedMenuItem {
  wxString text;
  wxString id;
  wxString tag;
  wxString onSelected;
};


class azMenuItem : public azWrapper {

public:

  azMenuItem(lua_State *L);
  ~azMenuItem();


  int getText(lua_State *L);
  int setText(lua_State *L);
  int getId(lua_State *L);
  int setId(lua_State *L);
  int getTag(lua_State *L);
  int setTag(lua_State *L);
  int getOnSelected(lua_State *L);
  int setOnSelected(lua_State *L);
  
  
  static const char className[];
  static Lunar<azMenuItem>::RegType methods[];

  azWrappedMenuItem* Get() const;

private:

  azMenuItem() {};
  azWrappedMenuItem* menuItem_;

};


#endif // __azMenuItem_H