/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "AboutDialog.h"
#include <wx/arrstr.h>
#include <wx/font.h>
#include "../utilities/StringUtil.h"
#include "../utilities/VersionInfo.h"
#include "../Localization.h"
#include "../FilePaths.h"
#include "../Constants.h"


BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
  EVT_BUTTON(ID_ABOUT_DLG_BUTTON_OK, AboutDialog::OnOkButtonClick)
  EVT_SHOW(AboutDialog::OnShow)
END_EVENT_TABLE()


AboutDialog::AboutDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
: AboutDialogBase(parent, id, title, pos, size, style) {
  iconBitmap->SetBitmap(wxBitmap(FilePaths::GetBaseSkinDirectory() + _T("/ApplicationIcon48.png"), wxBITMAP_TYPE_PNG));
  
  wxFont linkLabelFont(linkLabel->GetFont());
  linkLabelFont.SetUnderlined(true);
  linkLabel->SetFont(linkLabelFont);
  linkLabel->SetForegroundColour(wxColour(0,0,255));
  linkLabel->SetCursor(wxCursor(wxCURSOR_HAND));
  linkLabel->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(AboutDialog::OnAuthorLabelMouseDown), NULL, this);
  
  Localize();
}


void AboutDialog::OnAuthorLabelMouseDown(wxMouseEvent& evt) {
  ::wxLaunchDefaultBrowser(linkLabel->GetLabel(), wxBROWSER_NEW_WINDOW);
}


void AboutDialog::OnShow(wxShowEvent& evt) {
  iconBitmap->GetParent()->Layout();
}


void AboutDialog::Localize() {
  SetTitle(LOC1(_T("AboutDialog.Title"), APPLICATION_NAME));  
  notebook->SetPageText(0, LOC(_T("AboutDialog.AboutTab")));
  notebook->SetPageText(1, LOC(_T("AboutDialog.License")));
  okButton->SetLabel(LOC(_T("Global.OK")));
}


void AboutDialog::LoadContent() {
  Localize();

  wxString version = VersionInfo::GetVersionString();
  wxArrayString versionSplit;
  StringUtil::Split(version, versionSplit, _T("."));
  while (versionSplit.Count() < 4) versionSplit.Add(_T("0"));

  wxString versionText = wxString::Format(_T("%s, Version %s.%s.%s (Build #%s)"), APPLICATION_NAME, versionSplit[0], versionSplit[1], versionSplit[2], versionSplit[3]);
  versionLabel->SetValue(versionText);
}


void AboutDialog::OnOkButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_OK);
}


