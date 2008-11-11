/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#ifndef __MessageBoxes_H
#define __MessageBoxes_H


class MessageBoxes {

public:

  static int ShowWarning(const wxString& message, long style = wxOK, const wxString& checkBoxLabel = wxEmptyString, bool checkBoxState = false);
  static int ShowError(const wxString& message, long style = wxOK, const wxString& checkBoxLabel = wxEmptyString, bool checkBoxState = false);
  static int ShowInformation(const wxString& message, long style = wxOK, const wxString& checkBoxLabel = wxEmptyString, bool checkBoxState = false);
  static int ShowConfirmation(const wxString& message, long style = wxYES | wxNO, const wxString& checkBoxLabel = wxEmptyString, bool checkBoxState = false);
  static bool GetCheckBoxState();

};

#endif // __MessageBoxes_H