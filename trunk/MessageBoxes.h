#ifndef __MessageBoxes_H
#define __MessageBoxes_H

#include "wx/wx.h"


class MessageBoxes {

public:

  static int ShowWarning(const wxString& message, long style = wxOK);
  static int ShowError(const wxString& message, long style = wxOK);
  static int ShowConfirmation(const wxString& message, long style = wxYES | wxNO);

};

#endif // __MessageBoxes_H