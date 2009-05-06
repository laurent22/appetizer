/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"
#include "../PluginPreferences.h"

#include "PluginPreferencesDialog.h"


BEGIN_EVENT_TABLE(PluginPreferencesDialog, wxDialog)
  EVT_BUTTON(wxID_ANY, PluginPreferencesDialog::OnButtonClicked) 
END_EVENT_TABLE()


PluginPreferencesDialog::PluginPreferencesDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
: wxDialog(parent, id, title, pos, size, style) {



  //wxStaticText* infoLabel = new wxStaticText(this, wxID_ANY, wxString::Format(_("This tool will help you import your shortcuts into %s.\n\nPlease select below where you would like to import your shortcuts from:"), APPLICATION_NAME), wxPoint(x, y));
  //infoLabel->Wrap(windowWidth - border * 2);
  //wxSize infoSize = infoLabel->GetBestSize();
  //infoLabel->SetSize(infoSize);

  //y += infoSize.GetHeight() + gap * 2;

  //bool hasPortableAppsFolder = wxFileName::DirExists(FolderItem::ResolvePath(wxGetApp().GetUser()->GetSettings()->GetString(_T("PortableAppsPath"))));

  //wxArrayString sourceNames;
  //wxArrayString sourceLabels;
  //sourceNames.Add(_T("portableApps")); sourceLabels.Add(_("PortableApps.com applications"));
  //sourceNames.Add(_T("quickLaunch")); sourceLabels.Add(_("Windows Quick Launch toolbar"));
  //sourceNames.Add(_T("startupMenu")); sourceLabels.Add(_("Windows Start Menu programs"));

  //for (int i = 0; i < sourceNames.Count(); i++) {
  //  wxString n = sourceNames[i];
  //  wxString l = sourceLabels[i];

  //  wxCheckBox* b = new wxCheckBox(this, wxID_ANY, l);
  //  b->SetSize(x, y, windowWidth - gap * border, -1);
  //  b->SetName(n);

  //  choiceCheckBoxes.push_back(b);
  //  b->Connect(wxID_ANY, wxEVT_COMMAND_CHECKBOX_CLICKED, wxCommandEventHandler(ImportWizardDialog::OnCheckBoxClicked), NULL, this);

  //  if (n == _T("portableApps")) {
  //    b->Enable(hasPortableAppsFolder);
  //    if (hasPortableAppsFolder) b->SetValue(true);
  //  }

  //  if (n == _T("quickLaunch")) b->Enable(wxFileName::DirExists(FilePaths::GetQuickLaunchDirectory()));
  //  
  //  y += b->GetSize().GetHeight() + gap;
  //}

  //y += gap;

  //statusLabel = new wxStaticText(this, wxID_ANY, _T(" "), wxPoint(border, y), wxSize(windowWidth - gap * 2, -1));
  //wxFont font(statusLabel->GetFont());
  //font.SetWeight(wxBOLD);
  //statusLabel->SetFont(font);
  //
  //y += statusLabel->GetSize().GetHeight() + gap * 1.5;

  //startButton = new wxButton(this, ID_IMPORTWIZDLG_BUTTON_Start, _("Start >>"));
  //startButton->SetSize(startButton->GetBestSize());

  //cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
  //cancelButton->SetSize(cancelButton->GetBestSize());

  //startButton->SetPosition(wxPoint(windowWidth - border - startButton->GetSize().GetWidth(), y));
  //cancelButton->SetPosition(wxPoint(startButton->GetPosition().x - gap - cancelButton->GetSize().GetWidth(), y));

  //y += startButton->GetSize().GetHeight();

  //y += border;
  //SetClientSize(windowWidth, y);

  //OnCheckBoxClicked(wxCommandEvent());
}


PluginPreferencesDialog::~PluginPreferencesDialog() {
  for(int i = 0; i < controls_.size(); i++) {
    PluginPreferenceDialogControl* controlData = controls_.at(i);
    wxDELETE(controlData);
  }
  controls_.clear();
}


void PluginPreferencesDialog::LoadPreferences(Plugin* plugin) {
  SetTitle(_("Preferences"));

  plugin_ = plugin;
  PluginPreferences* preferences = plugin->GetPreferences();

  int border = 12;
  int gap = 8;
  int windowWidth = 400;

  wxBoxSizer* rootSizer = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* innerSizer = new wxBoxSizer(wxVERTICAL);
  mainSizer = new wxFlexGridSizer(preferences->Count(), 2, gap, gap);
  mainSizer->AddGrowableCol(1);
  wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  for (int i = 0; i < preferences->Count(); i++) {
    PluginPreference* preference = preferences->GetPreferenceAt(i);
    wxStaticText* label = new wxStaticText(this, wxID_ANY, preference->GetTitle());

    PluginPreferenceDialogControl* controlData = new PluginPreferenceDialogControl();

    wxWindow* control = NULL;

    if (preference->GetType() == PluginPreferenceType::Text) {

      control = new wxTextCtrl(this, wxID_ANY);
      wxTextCtrl* textBox = dynamic_cast<wxTextCtrl*>(control);
      textBox->SetValue(preference->GetValue());

    } else if (preference->GetType() == PluginPreferenceType::TextArea) {

      control = new wxTextCtrl(this, wxID_ANY);
      wxTextCtrl* textBox = dynamic_cast<wxTextCtrl*>(control);
      textBox->SetValue(preference->GetValue());

    } else if (preference->GetType() == PluginPreferenceType::Popup) {

      control = new wxComboBox(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 0, wxCB_READONLY);
      wxComboBox* comboBox = dynamic_cast<wxComboBox*>(control);

      PluginPreferenceOptions options = preference->GetOptions();
      PluginPreferenceOptions::iterator it;
      int selectedIndex = 0;
      int currentIndex = 0;
      
      for(it = options.begin(); it != options.end(); ++it) {
        wxStringClientData* data = new wxStringClientData(it->first);
        comboBox->Append(it->second, data);
        if (it->first == preference->GetValue()) {
          selectedIndex = currentIndex;
        }
        currentIndex++;
      }

      comboBox->Select(selectedIndex);

    }

    mainSizer->Add(label, 0, wxEXPAND, 0);
    mainSizer->Add(control, 0, wxEXPAND, 0);   

    controlData->control = control;
    controlData->label = label;

    controls_.push_back(controlData);
  }

  wxButton* saveButton = new wxButton(this, wxID_SAVE, _("Save"));
  wxButton* cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));

  buttonSizer->Add(saveButton, 0);
  buttonSizer->Add(cancelButton, 0, wxLEFT, gap);


  innerSizer->Add(mainSizer, 1, wxALL | wxEXPAND, border);
  innerSizer->Add(buttonSizer, 1, wxALL | wxALIGN_RIGHT, border);
  rootSizer->Add(innerSizer, 1, wxALL|wxEXPAND, 0);

  SetSizer(rootSizer);

  rootSizer->SetSizeHints(this);

  int windowHeight = GetSize().GetHeight();

  SetSize(windowWidth, windowHeight);
}


void PluginPreferencesDialog::OnButtonClicked(wxCommandEvent& evt) {
  switch (evt.GetId()) {

    case ID_PLUGINPREFDLG_BUTTON_Save:

      //statusLabel->SetLabel(_("Importing shortcuts... Please wait."));
      //cancelButton->Enable(false);
      //startButton->Enable(false);
      //Update();

      //for (int i = 0; i < choiceCheckBoxes.size(); i++) {
      //  wxCheckBox* c = choiceCheckBoxes.at(i);
      //  if (!c->GetValue()) continue;

      //  if (c->GetName() == _T("portableApps")) {
      //    wxGetApp().GetUser()->PortableAppsFormatSynchronization();
      //  } else if (c->GetName() == _T("quickLaunch")) {
      //    wxGetApp().GetUser()->QuickLaunchSynchronization();
      //  } else if (c->GetName() == _T("startupMenu")) {
      //    wxGetApp().GetUser()->StartMenuSynchronization();
      //  }
      //}

      EndModal(wxID_CLOSE);
      
      break;

    default:

      evt.Skip();

  }
}