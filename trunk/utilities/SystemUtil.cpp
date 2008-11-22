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