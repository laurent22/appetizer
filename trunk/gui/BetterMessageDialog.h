/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __BetterMessageDialog_H
#define __BetterMessageDialog_H


class BetterMessageDialog: public wxDialog {

public:

  BetterMessageDialog(
    const wxString& message, 
    const wxString& title = wxEmptyString,
    int style = wxOK,
    bool showCheckBox = false,
    bool checkBoxState = false,
    wxString checkBoxLabel = wxEmptyString,
    wxWindow* parent = NULL,
    int id = wxID_ANY,
    const wxPoint& pos = wxDefaultPosition,
    const wxSize& size = wxSize(400, 50),
    const wxString& name = _T("dialogBox"));

  static int ShowMessage(
    const wxString& message, 
    const wxString& title = wxEmptyString,
    int style = wxOK,
    bool showCheckBox = false,
    bool checkBoxState = false,
    wxString checkBoxLabel = wxEmptyString,
    wxWindow* parent = NULL);

  void OnButton(wxCommandEvent& evt);

  static bool GetCheckBoxState();
  static void DestroyInstance();

private:

  static bool lastCheckBoxState_;
  wxCheckBox* checkbox_;
  static BetterMessageDialog* messageDialog_;

  DECLARE_EVENT_TABLE()

};


#endif // __BetterMessageDialog_H