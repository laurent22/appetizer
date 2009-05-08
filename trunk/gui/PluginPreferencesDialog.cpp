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
  PluginPreferenceGroupVector preferenceGroups = preferences->GetPreferenceGroups();

  int border = 10;
  int gap = 8;
  int windowWidth = 400;

  wxBoxSizer* rootSizer = new wxBoxSizer(wxVERTICAL);
  wxNotebook* notebook = new wxNotebook(this, wxID_ANY);
  wxBoxSizer* bottomSizer = new wxBoxSizer(wxHORIZONTAL);


  std::map<wxString, wxPanel*> panels;
  std::map<wxString, wxFlexGridSizer*> panelGridSizers;


  int groupIndex = -1;
  int groupCount = preferenceGroups.size();

  while (groupIndex < groupCount) {
    wxString groupTitle;
    wxString groupName;
    int preferenceCount = 0;

    if (groupIndex == -1) {
      groupTitle = _("General");
      groupName = _T("general");
      preferenceCount = preferences->CountGroupPreferences(_T(""));
    } else {
      PluginPreferenceGroup* group = preferenceGroups.at(groupIndex);
      groupTitle = group->Title;
      groupName = group->Name;
      preferenceCount = preferences->CountGroupPreferences(group->Name);
    }

    wxPanel* panel = new wxPanel(notebook);
    notebook->AddPage(panel, groupTitle);
    panels[groupName] = panel;

    wxBoxSizer* boxSizer = new wxBoxSizer(wxHORIZONTAL);
    panel->SetSizer(boxSizer);

    wxFlexGridSizer* gridSizer = new wxFlexGridSizer(preferenceCount, 2, 8, 8);
    boxSizer->Add(gridSizer, 1, wxALL | wxEXPAND, 10);
    panelGridSizers[groupName] = gridSizer;
    gridSizer->AddGrowableCol(1);

    groupIndex++;
  }
  


  for (int i = 0; i < preferences->Count(); i++) {
    PluginPreference* preference = preferences->GetPreferenceAt(i);
    PluginPreferenceGroup* preferenceGroup = preference->GetGroup();

    wxPanel* groupPanel = NULL;
    wxFlexGridSizer* gridSizer = NULL;

    if (preferenceGroup) {
      groupPanel = panels[preferenceGroup->Name];
      gridSizer = panelGridSizers[preferenceGroup->Name];
    } else {
      groupPanel = panels[_T("general")];
      gridSizer = panelGridSizers[_T("general")];
    }

    wxStaticText* label = new wxStaticText(groupPanel, wxID_ANY, preference->GetTitle(), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER_VERTICAL);

    PluginPreferenceDialogControl* controlData = new PluginPreferenceDialogControl();

    wxWindow* control = NULL;

    if (preference->GetType() == PluginPreferenceType::Text) {

      control = new wxTextCtrl(groupPanel, wxID_ANY);
      wxTextCtrl* textBox = dynamic_cast<wxTextCtrl*>(control);
      textBox->SetValue(preference->GetValue());

    } else if (preference->GetType() == PluginPreferenceType::TextArea) {

      control = new wxTextCtrl(groupPanel, wxID_ANY);
      wxTextCtrl* textBox = dynamic_cast<wxTextCtrl*>(control);
      textBox->SetValue(preference->GetValue());

    } else if (preference->GetType() == PluginPreferenceType::Popup) {

      control = new wxComboBox(groupPanel, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 0, wxCB_READONLY);
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

    gridSizer->Add(label, 0, wxEXPAND, 0);
    gridSizer->Add(control, 0, wxEXPAND, 0);   

    controlData->control = control;
    controlData->label = label;
    controlData->preference = preference;

    controls_.push_back(controlData);

  }





  wxStaticText* bottomSizerFiller = new wxStaticText(this, wxID_ANY, _T(""));
  wxButton* saveButton = new wxButton(this, wxID_SAVE, _("Save"));
  wxButton* cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));


  bottomSizer->Add(bottomSizerFiller, 1, wxEXPAND);
  bottomSizer->Add(saveButton, 0, wxRIGHT | wxALIGN_BOTTOM, border);
  bottomSizer->Add(cancelButton, 0, wxALIGN_BOTTOM, 0);

  rootSizer->Add(notebook, 1, wxLEFT | wxRIGHT | wxTOP | wxEXPAND, 4);
  rootSizer->Add(bottomSizer, 0, wxALL | wxEXPAND | wxALIGN_RIGHT, border);




  SetSizer(rootSizer);
  rootSizer->SetSizeHints(this);

  SetSize(windowWidth, GetSize().GetHeight());

}


void PluginPreferencesDialog::OnButtonClicked(wxCommandEvent& evt) {
  switch (evt.GetId()) {

    case wxID_SAVE:

      for (int i = 0; i < controls_.size(); i++) {
        PluginPreferenceDialogControl* controlData = controls_.at(i);
        PluginPreference* preference = controlData->preference;
        
        wxComboBox* comboBox = dynamic_cast<wxComboBox*>(controlData->control);
        if (comboBox) {
          wxStringClientData* clientData = (wxStringClientData*)(comboBox->GetClientObject(comboBox->GetSelection()));
          preference->SetValue(clientData->GetData());
          continue;
        }

        wxTextCtrl* textBox = dynamic_cast<wxTextCtrl*>(controlData->control);
        if (textBox) {
          preference->SetValue(textBox->GetValue());
          continue;
        }
        
      }

      plugin_->GetPreferences()->ScheduleSave();

      EndModal(wxID_CLOSE);
      
      break;

    default:

      evt.Skip();

  }
}