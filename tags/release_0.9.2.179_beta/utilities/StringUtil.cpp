/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "StringUtil.h"


wxString StringUtil::ZeroPadding(int number, int digits) {
  wxString output;
  output << number;
  
  while (output.Len() < digits) output = _T("0") + output;

  return output;
}