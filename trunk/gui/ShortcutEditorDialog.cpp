#include "ShortcutEditorDialog.h"
#include "../Controller.h"
#include <wx/filename.h>



extern ControllerSP gController;


BEGIN_EVENT_TABLE(ShortcutEditorDialog, wxDialog)
  EVT_BUTTON(ID_BUTTON_Cancel, ShortcutEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_BUTTON_Save, ShortcutEditorDialog::OnSaveButtonClick)
END_EVENT_TABLE()


ShortcutEditorDialog::ShortcutEditorDialog()
: ShortcutEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {

}


void ShortcutEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  folderItem_ = folderItem;
  UpdateFromFolderItem();
}


void ShortcutEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  Close();
}


void ShortcutEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  wxString folderItemFilePath = locationTextBox->GetValue();
  wxString folderItemName = nameTextBox->GetValue();

  wxFileName filename(FolderItem::ResolvePath(folderItemFilePath));
  if (!filename.FileExists()) {
    // If the shortcut location doesn't exist, just show a warning but allow
    // the user to continue. Invalid shortcuts are allowed since they
    // might be referencing files from a different computer.
    int result = gController->ShowWarningMessage(_T("The shorcut location doesn't exist. Do you wish to continue?"), wxOK | wxCANCEL);
    if (result == wxID_CANCEL) return;
  }

  folderItem_->SetFilePath(folderItemFilePath);
  folderItem_->SetName(folderItemName);

  gController->GetUser()->ScheduleSave();

  Close();
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  locationTextBox->SetValue(folderItem_->GetFilePath());
}