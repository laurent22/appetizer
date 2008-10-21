#include "ShortcutEditorDialog.h"
#include "../Controller.h"



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
  
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  locationTextBox->SetValue(folderItem_->GetFilePath());
}