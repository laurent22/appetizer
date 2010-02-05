/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __ShortcutInfo_H
#define __ShortcutInfo_H


class ShortcutInfo {

public:

  ShortcutInfo(const wxString& shortcutPath, HWND windowHandle);
  ~ShortcutInfo();
  wxString GetDescription();
  wxString GetArguments();
  wxString GetPath();
  wxString GetIconLocation();
  int GetIconIndex();
  bool IsOk();

private:

  IShellLink* iShellLink_;
  IPersistFile* iPersistFile_; 
  bool isOk_;
  wxString filePath_;

};

#endif // __ShortcutInfo_H