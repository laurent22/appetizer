/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azWrapper_H
#define __azWrapper_H


class azWrapper: public wxEvtHandler {

public:

  azWrapper() {}
  azWrapper(lua_State *L) {}

  virtual wxObject* Get() { return NULL; }

  int CheckWrappedObject(lua_State *L, wxObject* wrappedObject);

  int LuaNullError(lua_State *L);
  int LuaPrivateConstructorError(lua_State *L);

};


#endif // __azWrapper_H