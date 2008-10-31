/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "AboutDialog.h"
#include "../Controller.h"
#include "../Localization.h"
#include "../Constants.h"

extern Controller gController;


BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
  EVT_BUTTON(ID_ABOUT_DLG_BUTTON_OK, AboutDialog::OnOkButtonClick)
END_EVENT_TABLE()


AboutDialog::AboutDialog()
: AboutDialogBase(NULL, wxID_ANY, wxEmptyString) {
  Localize();
}


void AboutDialog::Localize() {
  SetTitle(LOC1(_T("AboutDialog.Title"), APPLICATION_NAME));  
  notebook->SetPageText(0, LOC(_T("AboutDialog.AboutTab")));
  notebook->SetPageText(1, LOC(_T("AboutDialog.License")));
  okButton->SetLabel(LOC(_T("Global.OK")));
}


void AboutDialog::LoadContent() {
  Localize();

  wxString version = wxString::Format(_T("%s, Version %s"), APPLICATION_NAME, gController.GetVersionString());
  versionLabel->SetValue(version);
}


void AboutDialog::OnOkButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_OK);
}


