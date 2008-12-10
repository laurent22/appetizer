/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azSystem_H
#define __azSystem_H


#include "azWrapper.h"


class azSystem : public azWrapper {

public:

  azSystem();
  azSystem(lua_State *L);
  ~azSystem();

  int runCommand(lua_State *L);
  int killLockingProcesses(lua_State *L);	
  
  static const char className[];
  static Lunar<azSystem>::RegType methods[];

private:

  std::vector<wxProcess*> createdProcesses_;

};




class azSystemProcess : public wxProcess {

public:

  azSystemProcess(wxEvtHandler * parent, int id = -1);
  virtual void OnTerminate(int pid, int status); 

  wxString luaCallbackFunction;
  lua_State* luaState;
  wxArrayString commandOutput;

};





#endif // __azSystem_H