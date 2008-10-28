#include "ShortcutEditorDialog.h"
#include <wx/filename.h>
#include <wx/dirdlg.h>
#include <wx/filedlg.h>
#include "../Localization.h"
#include "../FilePaths.h"
#include "../Controller.h"
#include "../MessageBoxes.h"
#include "../utilities/DelphiToolsInterface.h"


extern Controller gController;


BEGIN_EVENT_TABLE(ShortcutEditorDialog, wxDialog)
  EVT_BUTTON(ID_BUTTON_Cancel, ShortcutEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_BUTTON_Save, ShortcutEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFile, ShortcutEditorDialog::OnSelectFileButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFolder, ShortcutEditorDialog::OnSelectFolderButtonClick)
END_EVENT_TABLE()


ShortcutEditorDialog::ShortcutEditorDialog()
: ShortcutEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {
  selectFolderButton->SetBitmapLabel(wxBitmap(FilePaths::IconsDirectory + _T("/FolderIcon.png"), wxBITMAP_TYPE_PNG));
  selectFileButton->SetBitmapLabel(wxBitmap(FilePaths::IconsDirectory + _T("/FileIcon.png"), wxBITMAP_TYPE_PNG));
  Localize();
}


void ShortcutEditorDialog::Localize() {
  SetTitle(LOC(_T("EditFolderItemForm.Title")));
  nameLabel->SetLabel(LOC(_T("EditFolderItemForm.NameLabel")));
  locationLabel->SetLabel(LOC(_T("EditFolderItemForm.LocationLabel")));
  saveButton->SetLabel(LOC(_T("Global.Save")));
  cancelButton->SetLabel(LOC(_T("Global.Cancel")));
}


void ShortcutEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  folderItem_ = folderItem;
  UpdateFromFolderItem();
}


void ShortcutEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_CANCEL);
}


void ShortcutEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  wxString folderItemFilePath = locationTextBox->GetValue();
  wxString folderItemName = nameTextBox->GetValue();

  wxFileName filename(FolderItem::ResolvePath(folderItemFilePath));
  if (!filename.FileExists() && !wxFileName::DirExists(folderItemFilePath)) {
    // If the shortcut location doesn't exist, just show a warning but allow
    // the user to continue. Invalid shortcuts are allowed since they
    // might be referencing files from a different computer.
    int result = MessageBoxes::ShowWarning(_T("The shorcut location doesn't exist. Do you wish to continue?"), wxOK | wxCANCEL);
    if (result == wxID_CANCEL) return;
  }

  folderItem_->SetFilePath(folderItemFilePath);
  folderItem_->SetName(folderItemName);

  EndDialog(wxID_OK);
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  locationTextBox->SetValue(folderItem_->GetFilePath());
}


void ShortcutEditorDialog::OnSelectFileButtonClick(wxCommandEvent& evt) {
  wxFileDialog fileDialog(this, _T("Pick a file"));
  int result = fileDialog.ShowModal();
  if (result != wxID_OK) return;

  wxString newValue = FolderItem::ConvertToRelativePath(fileDialog.GetPath());
  if (locationTextBox->GetValue() == newValue) return;

  locationTextBox->SetValue(newValue);

  wxString newName;
  DelphiToolsInterface::GetFileDescription(FolderItem::ResolvePath(newValue), newName);
  nameTextBox->SetValue(newName);
}


void ShortcutEditorDialog::OnSelectFolderButtonClick(wxCommandEvent& evt) {
  wxDirDialog dirDialog(this, _T("Pick a folder"));
  int result = dirDialog.ShowModal();
  if (result != wxID_OK) return;

  wxString newValue = FolderItem::ConvertToRelativePath(dirDialog.GetPath());
  if (locationTextBox->GetValue() == newValue) return;

  locationTextBox->SetValue(newValue);

  wxString newName;
  DelphiToolsInterface::GetFileDescription(FolderItem::ResolvePath(newValue), newName);
  nameTextBox->SetValue(newName);
}


