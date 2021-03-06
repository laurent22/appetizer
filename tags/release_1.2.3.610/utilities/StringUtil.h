﻿/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __StringUtil_H
#define __StringUtil_H


class StringUtil {

public:

  static wxString ZeroPadding(int number, int digits);
  static void Split(const wxString& toSplit, wxArrayString& resultArray, const wxString& delimiter);
  static wxString RemoveDriveFromPath(const wxString& path);
  static bool FileMatchesPattern(const wxString& pattern, const wxString& filePath);

};

#endif // __StringUtil_H