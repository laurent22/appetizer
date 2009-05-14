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


void SystemUtil::GetDirectoryContents(wxArrayString& fileItems, const wxString& directory, bool recurse) {
  int flags = wxDIR_FILES;
  if (recurse) flags |= wxDIR_DIRS;
  wxDir::GetAllFiles(directory, &fileItems, wxEmptyString, flags);
}