/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __ImportWizardDialog_H
#define __ImportWizardDialog_H


enum {
  ID_IMPORTWIZDLG_BUTTON_Start
};


class ImportWizardDialog: public wxDialog {

public:

  ImportWizardDialog(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("importWizardDialog"));
  ~ImportWizardDialog();

  void OnButtonClicked(wxCommandEvent& evt);
  void OnCheckBoxClicked(wxCommandEvent& evt);

protected:

  wxStaticText* statusLabel;
  wxButton* startButton;
  wxButton* cancelButton;

  std::vector<wxCheckBox*> choiceCheckBoxes; 

  DECLARE_EVENT_TABLE()

};


#endif // __ImportWizardDialog_H