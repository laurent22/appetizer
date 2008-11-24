// Written by Zoltan Csizmadia, zoltan_csizmadia@yahoo.com
// For companies(Austin,TX): If you would like to get my resume, send an email.
//
// The source is free, but if you want to use it, mention my name and e-mail address
//
//////////////////////////////////////////////////////////////////////////////////////
//
// NtSystemInfoTest.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <tchar.h>
#include <stdio.h>
#include "SystemInfo.h"


void ListModulesByDrive( DWORD processID, LPCTSTR lpFilter )
{
	BOOL show = TRUE;
	SystemModuleInformation mi( processID, TRUE );

	if ( mi.m_ModuleInfos.size() == 0 )
	{
		return;
	}
	
	// Iterating through the modules
	//for ( POSITION pos = mi.m_ModuleInfos.GetHeadPosition(); pos != NULL; )
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
   
				//show = _tcsicmp( fnameandext, lpFilter ) == 0;
        show = _tcsicmp( drive, lpFilter ) == 0;
			}
		}
		
		if ( show )
			_tprintf( _T("%d %s\n"), 
				m->ProcessId, 
				m->FullPath );
	}
}


void ListWindows( DWORD processID )
{
	SystemWindowInformation wi( processID, TRUE );

	if ( wi.m_WindowInfos.size() == 0)
	{
		//_tprintf( _T("No window information\n") );
		return;
	}
	
	//_tprintf( _T("%-6s  %-10s  %s\n"), _T("PID"), _T("Handle"), _T("Caption") );
	//_tprintf( _T("----------------------------------------------------------\n") );

	// Iterating through the windows
	//for ( POSITION pos = wi.m_WindowInfos.GetHeadPosition(); pos != NULL; )
	for (int i = 0; i < wi.m_WindowInfos.size(); i++)
  {
		SystemWindowInformation::WINDOW_INFO* w = wi.m_WindowInfos.at(i);//wi.m_WindowInfos.GetNext(pos);

		_tprintf( _T("%d %s\n"), 
				w->hWnd, 
				w->Caption );
		
		//_tprintf( _T("0x%04X  0x%08X  %s\n"), 
		//		w.ProcessId, 
		//		w.hWnd, 
		//		w.Caption );
	}
}


// Enable the SeDebugPrivilege
void EnableDebugPriv( void )
{
	HANDLE hToken;
	LUID sedebugnameValue;
	TOKEN_PRIVILEGES tkp;

	// enable the SeDebugPrivilege
	if ( ! OpenProcessToken( GetCurrentProcess(),
		TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken ) )
	{
		_tprintf( _T("OpenProcessToken() failed, Error = %d SeDebugPrivilege is not available.\n") , GetLastError() );
		return;
	}

	if ( ! LookupPrivilegeValue( NULL, SE_DEBUG_NAME, &sedebugnameValue ) )
	{
		_tprintf( _T("LookupPrivilegeValue() failed, Error = %d SeDebugPrivilege is not available.\n"), GetLastError() );
		CloseHandle( hToken );
		return;
	}

	tkp.PrivilegeCount = 1;
	tkp.Privileges[0].Luid = sedebugnameValue;
	tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	if ( ! AdjustTokenPrivileges( hToken, FALSE, &tkp, sizeof tkp, NULL, NULL ) )
		_tprintf( _T("AdjustTokenPrivileges() failed, Error = %d SeDebugPrivilege is not available.\n"), GetLastError() );
		
	CloseHandle( hToken );
}


int _tmain(int argc, TCHAR** argv)
{
  if (argc <= 1) return 999;

  if (strlen(argv[1]) <= 1) return 1000;

  TCHAR command = argv[1][1];
  if (command == NULL) return 1001;

	EnableDebugPriv();

  switch( command ) {

    case _T('d'):
    case _T('D'):

      {

      TCHAR* drive = argv[2];
      if (!drive) return 1002;

	    DWORD pID;
	    CStdString name;
	    SystemProcessInformation::SYSTEM_PROCESS_INFORMATION* p = NULL;

	    // Create process info object
	    SystemProcessInformation pi( TRUE );
    	
	    if ( pi.m_ProcessInfos.size() == 0 )
	    {
		    return 1003;
	    }
    	
      std::map< DWORD, SystemProcessInformation::SYSTEM_PROCESS_INFORMATION* >::iterator i = pi.m_ProcessInfos.begin();
      for( ; i != pi.m_ProcessInfos.end(); ++i )
      {
        DWORD processId = i->first;

        ListModulesByDrive(processId, drive);
      }
	    // Iterating through the processes
	    //for ( POSITION pos = pi.m_ProcessInfos.GetStartPosition(); pos != NULL; )
	    //{
		   // pi.m_ProcessInfos.GetNextAssoc( pos, pID, p ); 

     //   ListModulesByDrive(pID, drive);
	    //}

      }

      break;


    case _T('w'):
    case _T('W'):

      {

      if (!argv[2]) return 1004;

	    LONG processID;

	    if ( _tcsnicmp( argv[2], _T("0x"), 2 ) == 0 )
		    _stscanf( argv[2] + 2, "%x", &processID );
	    else
		    processID = atoi(argv[2]);

      ListWindows(processID);

      }

      break;

  }

	return 0;
}
