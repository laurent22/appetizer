/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "AboutDialog.h"
#include "../MiniLaunchBar.h"
#include "../utilities/StringUtil.h"
#include "../utilities/VersionInfo.h"
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
  linkLabel->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(AboutDialog::OnLinkLabelMouseDown), NULL, this);

  wxFont contactLabelFont(linkLabel->GetFont());
  contactLabelFont.SetUnderlined(true);
  contactLabel2->SetFont(contactLabelFont);
  contactLabel2->SetForegroundColour(wxColour(0,0,255));
  contactLabel2->SetCursor(wxCursor(wxCURSOR_HAND));
  contactLabel2->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(AboutDialog::OnContactLabelMouseDown), NULL, this);
  
  Localize();
}


void AboutDialog::OnLinkLabelMouseDown(wxMouseEvent& evt) {
  ::wxLaunchDefaultBrowser(linkLabel->GetLabel(), wxBROWSER_NEW_WINDOW);
}


void AboutDialog::OnContactLabelMouseDown(wxMouseEvent& evt) {
  ::wxLaunchDefaultBrowser(_T("mailto:") + wxGetApp().GetContactEmail(), wxBROWSER_NEW_WINDOW);
}


void AboutDialog::OnShow(wxShowEvent& evt) {
  iconBitmap->GetParent()->Layout();
}


void AboutDialog::Localize() {
  SetTitle(wxString::Format(_("About %s..."), APPLICATION_NAME));  
  notebook->SetPageText(0, _("About"));
  notebook->SetPageText(1, _("License"));
  okButton->SetLabel(_("OK"));
  creditLabel->SetLabel(_("Credits"));
  creditTextBox->SetValue(wxString::Format(
    _("Translators: %s\n\nThis software uses a number of open source libraries including:\n\n- Simpleini by Brodie Thiesfield\n- TinyXml by Yves Berquin\n- The Boost C++ Libraries and wxWidgets by their respective authors\n\nDialog boxes are built using wxGlade by Alberto Griggio."),
    _T("Simeon, Pieter Kerstens, Michał Trzebiatowski")));
  copyrightLabel->SetLabel(wxString::Format(_("Copyright © %s"), _T("2008 Laurent Cozic")));
  licenseTextBox->SetValue(_("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.\n\nThis program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>."));
  webLabel->SetLabel(_("Web:"));
  contactLabel->SetLabel(_("Contact:"));
  contactLabel2->SetLabel(wxGetApp().GetContactEmail());

  wxString version = VersionInfo::GetVersionString();
  wxArrayString versionSplit;
  StringUtil::Split(version, versionSplit, _T("."));
  while (versionSplit.Count() < 4) versionSplit.Add(_T("0"));

  wxString versionText = wxString::Format(_("%s, Version %s.%s.%s (Build #%s)"), APPLICATION_NAME, versionSplit[0], versionSplit[1], versionSplit[2], versionSplit[3]);
  versionLabel->SetValue(versionText);
}


void AboutDialog::LoadContent() {
  Localize();
}


void AboutDialog::OnOkButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_OK);
}


