/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azSystem.h"
#include "azGlobal.h"
#include "LuaUtil.h"
#include "../MiniLaunchBar.h"
#include "../FilePaths.h"




//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azSystem::className[] = "System";

#define method(class, name) {#name, &class::name}

Lunar<azSystem>::RegType azSystem::methods[] = {
  method(azSystem, runCommand),
  method(azSystem, killLockingProcesses),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azSystem::azSystem() {}


azSystem::~azSystem() {
  for (int i = 0; i < createdProcesses_.size(); i++) wxDELETE(createdProcesses_.at(i));
  createdProcesses_.clear();
}


azSystemProcess::azSystemProcess(wxEvtHandler* parent, int id) :
wxProcess(parent, id) {
  luaState = NULL;
}


void azSystemProcess::OnTerminate(int pid, int status) {
  wxLogDebug(_T("Process terminated: %d %d"), pid, status);

  if (!luaState) return;
  if (luaCallbackFunction == wxEmptyString) return;

  wxString output;
  wxInputStream* in = GetInputStream();
  
  if (in) {
    // do NOT use in->GetSize() - it always returns 0

    char buffer[256];

    while (true) {
      in->Read((void*)buffer, 255);
      int readCount = in->LastRead();
      buffer[readCount] = '\0';
      if (readCount > 0) {
        wxString conv(buffer, wxConvUTF8);
        output += conv;
      }
      
      if (in->Eof()) break;
    }
  }

  lua_State* L = luaState;
  
  lua_getfield(L, LUA_GLOBALSINDEX, luaCallbackFunction.mb_str());

  LuaHostTable parameters;
  parameters[_T("output")] = new LuaHostTableItem((wxObject*)&output, LHT_string);
  parameters[_T("sender")] = new LuaHostTableItem(&(wxGetApp()), LHT_wxObject);

  LuaUtil::ConvertAndPushLuaHostTable(L, parameters);
    
  int errorCode = lua_pcall(L, 1, 0, 0);

  if (errorCode) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8));
  }

}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azSystem::azSystem(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Use the 'system' global object instead.");
}


int azSystem::runCommand(lua_State *L) {
  wxString command = LuaUtil::ToString(L, 1);
  bool async = LuaUtil::ToBoolean(L, 2, true, true);
  wxString callback = LuaUtil::ToString(L, 3, true);

  long exitCode;

  if (async) {
    azSystemProcess* process = new azSystemProcess(NULL);
    process->Redirect();
    process->luaState = L;
    process->luaCallbackFunction = callback;

    createdProcesses_.push_back(process);

    exitCode = ::wxExecute(command, wxEXEC_NOHIDE | wxEXEC_ASYNC, process);

    luaHost_logInfo(wxString::Format(_T("Command exited with code %d"), exitCode));

  } else {   
    wxArrayString result;

    exitCode = ::wxExecute(command, result, wxEXEC_SYNC);

    luaHost_logInfo(wxString::Format(_T("Command exited with code %d"), exitCode));

    wxString output;
    for (int i = 0; i < result.Count(); i++) {
      if (output != wxEmptyString) output += _T("\n");
      output += result[i];
    }

    LuaUtil::PushString(L, output);

    return 1;
  }

  return 0;  
}


int azSystem::killLockingProcesses(lua_State *L) {
  wxString drive = LuaUtil::ToString(L, 1, true);
  if (drive == wxEmptyString) drive = FilePaths::GetApplicationDrive();
  bool painless = LuaUtil::ToBoolean(L, 2);

  wxGetApp().GetUtilities().KillLockingProcesses(drive, painless);

  return 0;
}

