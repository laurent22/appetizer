/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __AboutDialog_H
#define __AboutDialog_H


#include <wx/wx.h>
#include "AboutDialogBase.h"


class AboutDialog: public AboutDialogBase {

public:

  AboutDialog();
  void LoadContent();
  void Localize();

private:

  void OnOkButtonClick(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __AboutDialog_H