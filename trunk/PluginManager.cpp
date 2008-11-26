/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginManager.h"
#include "LuaWrapper.h"
#include "utilities/LuaUtil.h"


PluginManager::PluginManager() {
  PluginSP p(new Plugin());
  plugins_.push_back(p);

  p->LoadFile(_T("Data\\Plugins\\AddToGroupOnRightClick.lua"));

  //LuaHostTable m;
  //m[_T("un")] = _T("deux");
  //m[_T("11111")] = _T("quatre");
  //p->DispatchEvent(AZ_MAIN_FRAME_ID, 123, m);
}


PluginSP PluginManager::GetPluginByLuaState(lua_State* L) {
  for (int i = 0; i < plugins_.size(); i++) {
    if (plugins_.at(i)->GetLuaState() == L) return plugins_.at(i);
  }

  PluginSP nullOutput;
  return nullOutput;
}


void PluginManager::DispatchEvent(int objectId, int eventId, LuaHostTable arguments) {
  for (int i = 0; i < plugins_.size(); i++) {
    plugins_.at(i)->DispatchEvent(objectId, eventId, arguments);
  }
}