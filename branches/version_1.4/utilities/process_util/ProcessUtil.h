/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../../stdafx.h"


#ifndef __ProcessUtil_H
#define __ProcessUtil_H


#include "SystemInfo.h"


struct ProcessUtilModule {
  long id;
  CStdString path;
};


struct ProcessUtilWindow {
  HWND handle;
  CStdString title;
};


typedef std::vector<ProcessUtilModule*> ProcessUtilModuleVector;
typedef std::vector<ProcessUtilWindow*> ProcessUtilWindowVector;


class WindowsEnumerator {

public:

  WindowsEnumerator();
  ~WindowsEnumerator();
  void EnumerateForProcess(DWORD processId);

  DWORD searchedProcessId;
  ProcessUtilWindowVector windows;  

};


class ProcessUtil {

  public:

    static CStdString EnableDebugPriv();
    static void ListModulesByDrive( DWORD processID, LPCTSTR lpFilter, ProcessUtilModuleVector& moduleVector );
    static void ListAllModulesByDrive(LPCTSTR lpFilter, ProcessUtilModuleVector& moduleVector);
    //static void ListWindows( DWORD processID, ProcessUtilWindowVector& windowVector );

    static void DestroyProcessUtilModuleVector(ProcessUtilModuleVector& v);
    static void DestroyProcessUtilWindowVector(ProcessUtilWindowVector& v);

};

#endif // __ProcessUtil_H