/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef __VersionInfo_H
#define __VersionInfo_H


class VersionInfo {

public:

  static QString GetVersionString(const QString& filePath = "");
  static QString GetFileDescription(const QString& filePath);

};

#endif // __VersionInfo_H