/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __MessageBoxes_H
#define __MessageBoxes_H

#include "wx/wx.h"


class MessageBoxes {

public:

  static int ShowWarning(const wxString& message, long style = wxOK);
  static int ShowError(const wxString& message, long style = wxOK);
  static int ShowInformation(const wxString& message, long style = wxOK);
  static int ShowConfirmation(const wxString& message, long style = wxYES | wxNO);

};

#endif // __MessageBoxes_H