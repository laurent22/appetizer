/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../precompiled.h"

#ifndef __AboutDialog_H
#define __AboutDialog_H


#include "AboutDialogBase.h"


class AboutDialog: public AboutDialogBase {

public:

  AboutDialog(wxWindow* parent = NULL, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("dialogBox"));
  void LoadContent();
  void Localize();

private:

  void OnOkButtonClick(wxCommandEvent& evt);
  void OnShow(wxShowEvent& evt);
  void OnLinkLabelMouseDown(wxMouseEvent& evt);
  void OnContactLabelMouseDown(wxMouseEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __AboutDialog_H