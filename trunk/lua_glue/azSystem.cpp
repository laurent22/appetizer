/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/



/**
 * This class provides some system utility functions. Do not create
 * an instance of this class - instead use the global <code>system</code> object.
 * 
 * @see Global#system
 *
 */	



#include "../stdafx.h"

#include "azSystem.h"
#include "azGlobal.h"
#include "LuaUtil.h"
#include "../FolderItem.h"
#include "../utilities/SystemUtil.h"
#include "../utilities/StringUtil.h"
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
  method(azSystem, getDirectoryContents),
  method(azSystem, fileMatchesPattern),
  method(azSystem, resolvePath),
  method(azSystem, getLastCommandErrorCode),  
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azSystem::azSystem() {
  lastCommandErrorCode_ = 0;
}


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


/**
 * Allows running the given command line. It may be a full DOS command with arguments, or 
 * simply the path to an executable. The command can be run synchronously or asynchronously.
 * If it runs synchronously, the function only returns when the command has been executed. 
 * In this case, it also returns the command line standard output.
 * On the other hand, if the command is run asynchronously, the function returns immediately.
 * You may then provide a callback function that will be called when the command has finished.
 * The output can then be retrieved from the <code>event.output</code> property of the function parameter.
 * @param String commandLine The command line to execute.
 * @param Boolean asynchronous Sets this to <code>true</code> for asynchronous execution. (default false)
 * @param String functionName Name of the callback function in asynchronous mode. (default "")
 * @example This script calls a DOS command in synchronous mode.
 * <listing version="3.0">
 * -- Running command in synchronous mode
 * result = system:runCommand('cmd /c "dir c: /On"')
 * 
 * -- The function returns the result when the command is complete. 
 * dialogs:showMessage(result)
 * </listing>
 *
 * @example This script calls a DOS command in asynchronous mode with callback.
 * <listing version="3.0">
 * -- Define a function to retrieve the output
 * -- of the asynchronous command
 * 
 * function command_callback(event)
 *     -- The output is in event.output
 *     dialogs:showMessage(event.output)
 * end
 * 
 * -- Running command in asynchronous mode
 * system:runCommand('cmd /c "dir c: /On"', true, "command_callback")
 *
 * -- Note that the function does not return anything in that case
 * </listing>
 * 
 */	
int azSystem::runCommand(lua_State *L) {
  wxString command = LuaUtil::ToString(L, 1);
  bool async = LuaUtil::ToBoolean(L, 2, true, false);
  wxString callback = LuaUtil::ToString(L, 3, true);

  long exitCode;

  if (async) {

    azSystemProcess* process = new azSystemProcess(NULL);
    process->Redirect();
    process->luaState = L;
    process->luaCallbackFunction = callback;

    createdProcesses_.push_back(process);

    exitCode = ::wxExecute(command, wxEXEC_NOHIDE | wxEXEC_ASYNC, process);
    lastCommandErrorCode_ = exitCode;

    luaHost_logInfo(wxString::Format(_T("Command exited with code %d"), exitCode));

  } else {   

    wxArrayString result;

    exitCode = ::wxExecute(command, result, wxEXEC_SYNC);
    lastCommandErrorCode_ = exitCode;

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


int azSystem::getLastCommandErrorCode(lua_State *L) {
  lua_pushinteger(L, lastCommandErrorCode_);

  return 1;
}


/**
 * Kills the applications that are locking (have an open handle on) the given drive. If no drive is specified,
 * the drive Appetizer is on will be used instead. The function has two modes:
 * "safe" and "forced". In safe mode, the function tries to close the application windows properly, giving
 * them a chance to save any settings or to display a message if changes need to be saved. The drawback
 * is that some applications will not be closed. On the other hand, the "forced" mode is nearly guaranteed to close
 * all the applications. In that case, the function will do two passes: one in "safe" mode where it tries to close the applications
 * properly, and a second one where it kills the processes of the remaining applications (if any).
 * @param String drive Drive that needs to be unlocked. (default null)
 * @param Boolean safe Sets this to <code>true</code> for the "safe" mode or <code>false</code> for the "forced" mode. (default true)
 * 
 */	
int azSystem::killLockingProcesses(lua_State *L) {
  wxString drive = LuaUtil::ToString(L, 1, true);
  if (drive == wxEmptyString) drive = FilePaths::GetApplicationDrive();
  bool safe = LuaUtil::ToBoolean(L, 2, true, true);

  wxGetApp().GetUtilities().KillLockingProcesses(drive, safe);

  return 0;
}


int azSystem::getDirectoryContents(lua_State *L) {
  wxString inputDirectory = LuaUtil::ToString(L, 1);
  bool inputRecurse = LuaUtil::ToBoolean(L, 2, true, false);
  
  wxArrayString files;
  SystemUtil::GetDirectoryContents(files, inputDirectory, inputRecurse);

  lua_createtable(L, files.Count(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < files.Count(); i++) {
    wxString file = files[i];

    lua_pushinteger(L, i + 1);
    LuaUtil::PushString(L, file);
    lua_settable(L, tableIndex);
  }
  
  return 1;
}


int azSystem::fileMatchesPattern(lua_State *L) {
  wxString inputFilename = LuaUtil::ToString(L, 1);
  wxString inputPattern = LuaUtil::ToString(L, 2);

  bool itDoes = StringUtil::FileMatchesPattern(inputPattern, inputFilename);

  lua_pushboolean(L, itDoes);

  return 1;
}


int azSystem::resolvePath(lua_State *L) {
  wxString inputFilePath = LuaUtil::ToString(L, 1);
  bool normalize = LuaUtil::ToBoolean(L, 2, true, false);

  wxString output = FolderItem::ResolvePath(inputFilePath, normalize);

  LuaUtil::PushString(L, output);

  return 1;
}
