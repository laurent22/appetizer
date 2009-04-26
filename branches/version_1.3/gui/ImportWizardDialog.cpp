/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "ImportWizardDialog.h"
#include "../MiniLaunchBar.h"
#include "../FolderItem.h"
#include "../FilePaths.h"
#include "../Constants.h"


BEGIN_EVENT_TABLE(ImportWizardDialog, wxDialog)
  EVT_BUTTON(wxID_ANY, ImportWizardDialog::OnButtonClicked) 
END_EVENT_TABLE()


ImportWizardDialog::ImportWizardDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
: wxDialog(parent, id, title, pos, size, style) {

  SetTitle(_("Import shortcuts"));

  int border = 12;
  int gap = 8;
  int windowWidth = 320;
  int y = border;
  int x = border;

  wxStaticText* infoLabel = new wxStaticText(this, wxID_ANY, wxString::Format(_("This tool will help you import your shortcuts into %s.\n\nPlease select below where you would like to import your shortcuts from:"), APPLICATION_NAME), wxPoint(x, y));
  infoLabel->Wrap(windowWidth - border * 2);
  wxSize infoSize = infoLabel->GetBestSize();
  infoLabel->SetSize(infoSize);

  y += infoSize.GetHeight() + gap * 2;

  bool hasPortableAppsFolder = wxFileName::DirExists(FolderItem::ResolvePath(wxGetApp().GetUser()->GetSettings()->GetString(_T("PortableAppsPath"))));

  wxArrayString sourceNames;
  wxArrayString sourceLabels;
  sourceNames.Add(_T("portableApps")); sourceLabels.Add(_("PortableApps.com applications"));
  sourceNames.Add(_T("quickLaunch")); sourceLabels.Add(_("Windows Quick Launch toolbar"));
  sourceNames.Add(_T("startupMenu")); sourceLabels.Add(_("Windows Start Menu programs"));

  for (int i = 0; i < sourceNames.Count(); i++) {
    wxString n = sourceNames[i];
    wxString l = sourceLabels[i];

    wxCheckBox* b = new wxCheckBox(this, wxID_ANY, l);
    b->SetSize(x, y, windowWidth - gap * border, -1);
    b->SetName(n);

    choiceCheckBoxes.push_back(b);
    b->Connect(wxID_ANY, wxEVT_COMMAND_CHECKBOX_CLICKED, wxCommandEventHandler(ImportWizardDialog::OnCheckBoxClicked), NULL, this);

    if (n == _T("portableApps")) {
      b->Enable(hasPortableAppsFolder);
      if (hasPortableAppsFolder) b->SetValue(true);
    }

    if (n == _T("quickLaunch")) b->Enable(wxFileName::DirExists(FilePaths::GetQuickLaunchDirectory()));
    
    y += b->GetSize().GetHeight() + gap;
  }

  y += gap;

  statusLabel = new wxStaticText(this, wxID_ANY, _T(" "), wxPoint(border, y), wxSize(windowWidth - gap * 2, -1));
  wxFont font(statusLabel->GetFont());
  font.SetWeight(wxBOLD);
  statusLabel->SetFont(font);
  
  y += statusLabel->GetSize().GetHeight() + gap * 1.5;

  startButton = new wxButton(this, ID_IMPORTWIZDLG_BUTTON_Start, _("Start >>"));
  startButton->SetSize(startButton->GetBestSize());

  cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
  cancelButton->SetSize(cancelButton->GetBestSize());

  startButton->SetPosition(wxPoint(windowWidth - border - startButton->GetSize().GetWidth(), y));
  cancelButton->SetPosition(wxPoint(startButton->GetPosition().x - gap - cancelButton->GetSize().GetWidth(), y));

  y += startButton->GetSize().GetHeight();

  y += border;
  SetClientSize(windowWidth, y);

  OnCheckBoxClicked(wxCommandEvent());
}


ImportWizardDialog::~ImportWizardDialog() {

}


void ImportWizardDialog::OnCheckBoxClicked(wxCommandEvent& evt) {
  bool oneTicked = false;
  for (int i = 0; i < choiceCheckBoxes.size(); i++) {
    wxCheckBox* c = choiceCheckBoxes.at(i);
    if (!c->GetValue()) continue;
    oneTicked = true;
    break;
  }

  startButton->Enable(oneTicked);
}


void ImportWizardDialog::OnButtonClicked(wxCommandEvent& evt) {
  switch (evt.GetId()) {

    case ID_IMPORTWIZDLG_BUTTON_Start:

      statusLabel->SetLabel(_("Importing shortcuts... Please wait."));
      cancelButton->Enable(false);
      startButton->Enable(false);
      Update();

      for (int i = 0; i < choiceCheckBoxes.size(); i++) {
        wxCheckBox* c = choiceCheckBoxes.at(i);
        if (!c->GetValue()) continue;

        if (c->GetName() == _T("portableApps")) {
          wxGetApp().GetUser()->PortableAppsFormatSynchronization();
        } else if (c->GetName() == _T("quickLaunch")) {
          wxGetApp().GetUser()->QuickLaunchSynchronization();
        } else if (c->GetName() == _T("startupMenu")) {
          wxGetApp().GetUser()->StartMenuSynchronization();
        }
      }

      EndModal(wxID_CLOSE);
      
      break;

    default:

      evt.Skip();

  }
}