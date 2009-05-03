/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"
#include "../Plugin.h"

class Plugin;


#ifndef __PluginPreferencesDialog_H
#define __PluginPreferencesDialog_H


enum {
  ID_PLUGINPREFDLG_BUTTON_Save
};


class PluginPreferencesDialog: public wxDialog {

public:

  PluginPreferencesDialog(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("importWizardDialog"));
  ~PluginPreferencesDialog();
  void LoadPreferences(Plugin* plugin);

  void OnButtonClicked(wxCommandEvent& evt);
  void OnCheckBoxClicked(wxCommandEvent& evt);

protected:

  wxGridSizer* mainSizer;

  Plugin* plugin_;

  wxButton* saveButton;
  wxButton* cancelButton;

  DECLARE_EVENT_TABLE()

};


#endif // __PluginPreferencesDialog_H