/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"
#include "../PluginPreferences.h"
#include "../PluginPreference.h"


#ifndef __PluginPreferencesDialog_H
#define __PluginPreferencesDialog_H


enum {
  ID_PLUGINPREFDLG_BUTTON_Save
};


struct PluginPreferenceDialogControl {
  wxStaticText* label;
  wxWindow* control;
  PluginPreference* preference;
  wxStaticText* descriptionLabel;
};


typedef std::vector<PluginPreferenceDialogControl*> PluginPreferenceDialogControls;


class PluginPreferencesDialog: public wxDialog {

public:

  PluginPreferencesDialog(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("importWizardDialog"));
  ~PluginPreferencesDialog();
  void LoadPreferences(PluginPreferences* preferences, bool flatView = false, const wxString& saveButtonLabel = _("OK"));

  void OnButtonClicked(wxCommandEvent& evt);
  void OnCheckBoxClicked(wxCommandEvent& evt);

protected:

  
  wxFlexGridSizer* mainSizer;
  PluginPreferenceDialogControls controls_;

  PluginPreferences* preferences_;

  std::map<wxButton*, wxTextCtrl*> browseButtonToTextBoxMap_;

  wxButton* saveButton;
  wxButton* cancelButton;

  DECLARE_EVENT_TABLE()

};


#endif // __PluginPreferencesDialog_H