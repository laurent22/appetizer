/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "DelphiToolsInterface.h"
#include "../FilePaths.h"
#include <wx/filename.h>


HINSTANCE DelphiToolsInterface::hInstLibrary;
bool DelphiToolsInterface::IsLoaded = false;


void DelphiToolsInterface::LoadDLL() {
  if (DelphiToolsInterface::IsLoaded) return;
  DelphiToolsInterface::hInstLibrary = LoadLibrary(FilePaths::DataDirectory + _T("\\DelphiTools.dll"));
  DelphiToolsInterface::IsLoaded = true;
}


void DelphiToolsInterface::UnloadDLL() {
  if (!DelphiToolsInterface::IsLoaded) return;
  FreeLibrary(DelphiToolsInterface::hInstLibrary);
  DelphiToolsInterface::IsLoaded = false;
}


void DelphiToolsInterface::GetFileDescription(const wxString& filePath, wxString& fileDescription) {
  #ifdef __WIN32__

  typedef wxChar* (*GetVersionInfo_FileDescription)(const wxChar*);
  GetVersionInfo_FileDescription _GetVersionInfo_FileDescription;

  //HINSTANCE hInstLibrary = LoadLibrary(_T("Data\\DelphiTools.dll"));

  if (hInstLibrary) {
    _GetVersionInfo_FileDescription = (GetVersionInfo_FileDescription)GetProcAddress(hInstLibrary, "GetVersionInfo_FileDescription");
    wxChar* outputChar = _GetVersionInfo_FileDescription(filePath.c_str());
    //FreeLibrary(hInstLibrary);

    fileDescription = wxString(outputChar);

    // If we got the file description, exit now
    if (fileDescription != _T("")) return;

  } else {
    wxLogDebug(_T("DelphiTools.dll failed to load!"));
  }

  #endif // __WIN32__

  // If we couldn't get the file description field, return the
  // filename (without the extension)
  wxFileName filename(filePath);
  fileDescription = filename.GetName();
}