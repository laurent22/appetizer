/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __StringUtil_H
#define __StringUtil_H

#include <wx/wx.h>

class StringUtil {

public:

  static wxString ZeroPadding(int number, int digits);

};

#endif // __StringUtil_H