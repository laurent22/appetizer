/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "MessageBoxes.h"
#include <wx/msgdlg.h>
#include "MainFrame.h"


extern MainFrame* gMainFrame;


int MessageBoxes::ShowError(const wxString& message, long style) {
  wxMessageDialog dialog(gMainFrame, message, _("Error"), style | wxICON_ERROR);
  return dialog.ShowModal();
}


int MessageBoxes::ShowInformation(const wxString& message, long style) {
  wxMessageDialog dialog(gMainFrame, message, _("Information"), style | wxICON_INFORMATION);
  return dialog.ShowModal();
}


int MessageBoxes::ShowWarning(const wxString& message, long style) {
  wxMessageDialog dialog(gMainFrame, message, _("Warning "), style | wxICON_EXCLAMATION);
  return dialog.ShowModal();
}


int MessageBoxes::ShowConfirmation(const wxString& message, long style) {
  wxMessageDialog dialog(gMainFrame, message, _("Confirmation"), style | wxICON_QUESTION);
  return dialog.ShowModal();
}