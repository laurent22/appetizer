/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azIcon_H
#define __azIcon_H


#include "../FolderItemRenderer.h"


class azIcon {

public:

  azIcon(FolderItemRenderer* r) { renderer_ = r; }
  azIcon(lua_State *L) {}
  
  int getPopupMenu(lua_State *L);
  int getShortcut(lua_State *L);
  
  static const char className[];
  static Lunar<azIcon>::RegType methods[];

private:

  FolderItemRenderer* renderer_;

};


#endif // __azIcon_H