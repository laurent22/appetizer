/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __DelphiToolsInterface_H
#define __DelphiToolsInterface_H

#include "wx/wx.h"


class DelphiToolsInterface {

public:

  static bool IsLoaded;
  static HINSTANCE hInstLibrary;
  static void LoadDLL();
  static void UnloadDLL();
  static void GetFileDescription(const wxString& filePath, wxString& fileDescription);

};


#endif // __DelphiToolsInterface_H