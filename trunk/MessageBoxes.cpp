#include "MessageBoxes.h"
#include <wx/msgdlg.h>


int MessageBoxes::ShowError(const wxString& message, long style) {
  wxMessageDialog dialog(NULL, message, _T("Error"), style | wxICON_ERROR);
  return dialog.ShowModal();
}


int MessageBoxes::ShowWarning(const wxString& message, long style) {
  wxMessageDialog dialog(NULL, message, _T("Warning"), style | wxICON_EXCLAMATION);
  return dialog.ShowModal();
}