/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.

  Some of this code comes from NtSysInfo by Zoltan Csizmadia. Here is the original license:

  // Written by Zoltan Csizmadia, zoltan_csizmadia@yahoo.com
  // For companies(Austin,TX): If you would like to get my resume, send an email.
  //
  // The source is free, but if you want to use it, mention my name and e-mail address

  Modified by Laurent Cozic for use with Appetizer http://app.etizer.org
  The following changes have been made:

  24/11/2008:

  - Removed MFC dependency (Replaced CString by CStdString, CList by std::vector,
    CMap by std::map, and afxtempl.h by windows.h)
  - Added ListModulesByDrive() and ListAllModulesByDrive()
  - Fixed a bug whereas NtQuerySystemInformation would fail when too many
    processes were running simultaneously
  - Cleaned up header files (removed stdafx.h and other files that weren't used)

  25/11/2008:

  - Removed ListWindows() method and replaced it by WindowsEnumerator class,
    which uses native Win32 API calls.

*/


#include "../../stdafx.h"

#include "ProcessUtil.h"



WindowsEnumerator::WindowsEnumerator() {

}


WindowsEnumerator::~WindowsEnumerator() {
  ProcessUtil::DestroyProcessUtilWindowVector(windows);
}


BOOL CALLBACK WindowsEnumerator_EnumWindowsProc(HWND hwnd, LPARAM lParam) {
  WindowsEnumerator* that = (WindowsEnumerator*)lParam;

  DWORD processId;
  GetWindowThreadProcessId(hwnd, &processId);
  
  if (that->searchedProcessId != processId) return true;

  TCHAR title[255];
  GetWindowText(hwnd, title,	255);

  ProcessUtilWindow* w = new ProcessUtilWindow();
  w->handle = hwnd;
  w->title = title;

  that->windows.push_back(w);

  return true;
}


void WindowsEnumerator::EnumerateForProcess(DWORD processId) {
  searchedProcessId = processId;
  EnumWindows(WindowsEnumerator_EnumWindowsProc, (LPARAM)this);
}


// Enable the SeDebugPrivilege
CStdString ProcessUtil::EnableDebugPriv()
{
	HANDLE hToken;
	LUID sedebugnameValue;
	TOKEN_PRIVILEGES tkp;

	// enable the SeDebugPrivilege
	if ( ! OpenProcessToken( GetCurrentProcess(),
		TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken ) )
	{
    return _T("OpenProcessToken() failed, SeDebugPrivilege is not available.");
	}

	if ( ! LookupPrivilegeValue( NULL, SE_DEBUG_NAME, &sedebugnameValue ) )
	{
    CloseHandle( hToken );
		return _T("LookupPrivilegeValue() failed, SeDebugPrivilege is not available.");
	}

	tkp.PrivilegeCount = 1;
	tkp.Privileges[0].Luid = sedebugnameValue;
	tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

  if ( ! AdjustTokenPrivileges( hToken, FALSE, &tkp, sizeof tkp, NULL, NULL ) ) {
    CloseHandle(hToken);
		return _T("AdjustTokenPrivileges() failed, SeDebugPrivilege is not available.");
  }

	CloseHandle(hToken);

  return _T("ok");
}


void ProcessUtil::ListModulesByDrive( DWORD processID, LPCTSTR lpFilter, ProcessUtilModuleVector& moduleVector )
{
	BOOL show = TRUE;
	SystemModuleInformation mi( processID, TRUE );

	if ( mi.m_ModuleInfos.size() == 0 )
	{
		return;
	}
	
	// Iterating through the modules
  for (int i = 0; i < mi.m_ModuleInfos.size(); i++)
	{
		SystemModuleInformation::MODULE_INFO* m = mi.m_ModuleInfos.at(i);

		// Module name filtering
		if ( lpFilter == NULL )
			show = TRUE;
		else
		{
			if ( lpFilter[0] == _T('\0') )
				show = TRUE;
			else
			{
				TCHAR drive[_MAX_DRIVE];
				TCHAR dir[_MAX_DIR];
				TCHAR fname[_MAX_FNAME];
				TCHAR ext[_MAX_EXT];
				TCHAR fnameandext[_MAX_PATH];

				_tsplitpath( m->FullPath, drive, dir, fname, ext );

				_tcscpy( fnameandext, fname );
				_tcscat( fnameandext, ext );

        show = _tcsicmp( drive, lpFilter ) == 0;
			}
		}
		

    if (show) {
      ProcessUtilModule* output = new ProcessUtilModule();
      output->path = m->FullPath;
      output->id = m->ProcessId;
      moduleVector.push_back(output);
    }

	}
}


void ProcessUtil::ListAllModulesByDrive(LPCTSTR lpFilter, ProcessUtilModuleVector& moduleVector) {
  // Create process info object
  SystemProcessInformation pi(TRUE);
	
  if ( pi.m_ProcessInfos.size() != 0 )
  {
	
    std::map<DWORD, SystemProcessInformation::SYSTEM_PROCESS_INFORMATION*>::iterator i = pi.m_ProcessInfos.begin();
    for( ; i != pi.m_ProcessInfos.end(); ++i )
    {
      DWORD processId = i->first;
      ProcessUtil::ListModulesByDrive(processId, lpFilter, moduleVector);
    }

  } 
}


void ProcessUtil::DestroyProcessUtilModuleVector(ProcessUtilModuleVector& v) {
  for (int i = 0; i < v.size(); i++) delete v.at(i);
  v.clear();
}


void ProcessUtil::DestroyProcessUtilWindowVector(ProcessUtilWindowVector& v) {
  for (int i = 0; i < v.size(); i++) delete v.at(i);
  v.clear();
}