/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "SystemUtil.h"



bool SystemUtil::IsPathADrive(const wxString& path) {
  #ifdef __WINDOWS__
  return GetDriveType(path) > 1;
  #endif

  return false;
}


SystemUtilProcessVector SystemUtil::GetProcessList() {
  SystemUtilProcessVector output;

  #ifdef __WINDOWS__

  DWORD aProcesses[2048], cbNeeded, cProcesses;

  if (EnumProcesses(aProcesses, sizeof(aProcesses), &cbNeeded)) {
    // Calculate how many process identifiers were returned.
    cProcesses = cbNeeded / sizeof(DWORD);

    // Print the name and process identifier for each process.
    for (int i = 0; i < cProcesses; i++) {
      DWORD processId = aProcesses[i];
      if (processId == 0) continue;

      TCHAR szProcessName[MAX_PATH] = _T("");

      // Get a handle to the process.
      HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION |
                                    PROCESS_VM_READ,
                                    FALSE, processId);

      // Get the process name.
      if (hProcess) {
        HMODULE hMod;
        DWORD cbNeeded;

        if (EnumProcessModules(hProcess, &hMod, sizeof(hMod), &cbNeeded)) {
            GetModuleBaseName(hProcess, hMod, szProcessName, 
                              sizeof(szProcessName) / sizeof(TCHAR));
        }
      }

      SystemUtilProcess* p = new SystemUtilProcess();
      p->id = processId;
      p->name = wxString(szProcessName, wxConvUTF8);

      output.push_back(p);

      CloseHandle(hProcess);
    }

  }    

  #endif // __WINDOWS__

  return output;
}