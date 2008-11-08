/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __VersionInfo_H
#define __VersionInfo_H

#include <wx/wx.h>

#ifdef __WINDOWS__
#include <windows.h>
#include <winver.h>
#endif // __WINDOWS__


class VersionInfo {

public:

  static wxString GetVersionString(const wxString& filePath = wxEmptyString);
  static wxString GetFileDescription(const wxString& filePath);

};

#endif // __VersionInfo_H