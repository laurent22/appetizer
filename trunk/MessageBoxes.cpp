/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#include "MessageBoxes.h"
#include "MiniLaunchBar.h"
#include "gui/BetterMessageDialog.h"


int MessageBoxes::ShowError(const wxString& message, long style, const wxString& checkBoxLabel, bool checkBoxState) {
  return BetterMessageDialog::ShowMessage(message, _("Error"), style | wxICON_ERROR, checkBoxLabel != wxEmptyString, checkBoxState, checkBoxLabel, wxGetApp().GetMainFrame());
}


int MessageBoxes::ShowInformation(const wxString& message, long style, const wxString& checkBoxLabel, bool checkBoxState) {  
  return BetterMessageDialog::ShowMessage(message, _("Information"), style | wxICON_INFORMATION, checkBoxLabel != wxEmptyString, checkBoxState, checkBoxLabel, wxGetApp().GetMainFrame());
}


int MessageBoxes::ShowWarning(const wxString& message, long style, const wxString& checkBoxLabel, bool checkBoxState) {
  return BetterMessageDialog::ShowMessage(message, _("Warning"), style | wxICON_EXCLAMATION, checkBoxLabel != wxEmptyString, checkBoxState, checkBoxLabel, wxGetApp().GetMainFrame());
}


int MessageBoxes::ShowConfirmation(const wxString& message, long style, const wxString& checkBoxLabel, bool checkBoxState) {
  return BetterMessageDialog::ShowMessage(message, _("Confirmation"), style | wxICON_QUESTION, checkBoxLabel != wxEmptyString, checkBoxState, checkBoxLabel, wxGetApp().GetMainFrame());
}


bool MessageBoxes::GetCheckBoxState() {
  return BetterMessageDialog::GetCheckBoxState();
}