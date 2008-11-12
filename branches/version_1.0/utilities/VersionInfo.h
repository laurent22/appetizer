/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../precompiled.h"

#ifndef __VersionInfo_H
#define __VersionInfo_H


class VersionInfo {

public:

  static wxString GetVersionString(const wxString& filePath = wxEmptyString);
  static wxString GetFileDescription(const wxString& filePath);

};

#endif // __VersionInfo_H