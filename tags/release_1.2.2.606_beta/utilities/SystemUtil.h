/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __SystemUtil_H
#define __SystemUtil_H


struct SystemUtilProcess {
  wxString name;
  long id;
};

typedef std::vector<SystemUtilProcess*> SystemUtilProcessVector;


class SystemUtil {

public:

  static bool IsPathADrive(const wxString& path);
  static SystemUtilProcessVector GetProcessList();

};

#endif // __SystemUtil_H