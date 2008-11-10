/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "GroupEditorDialog.h"
#include <wx/filename.h>
#include <wx/dirdlg.h>
#include <wx/filedlg.h>
#include "../FilePaths.h"
#include "../MiniLaunchBar.h"
#include "../MessageBoxes.h"
#include "../utilities/VersionInfo.h"


BEGIN_EVENT_TABLE(GroupEditorDialog, wxDialog)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_Cancel, GroupEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_Save, GroupEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_ChangeIcon, GroupEditorDialog::OnChangeIconButtonClick)  
  EVT_SHOW(GroupEditorDialog::OnShow)
END_EVENT_TABLE()


GroupEditorDialog::GroupEditorDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
: GroupEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {
  Localize();
}


void GroupEditorDialog::Localize() {
  SetTitle(_("Shorcut Properties"));
  nameLabel->SetLabel(_("Name:"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));
}


void GroupEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  hasSelectedDefaultIcon_ = false;
  folderItem_ = folderItem;
  UpdateFromFolderItem();
}


void GroupEditorDialog::OnMenuItemClick(wxCommandEvent& evt) {
  int itemId = evt.GetId();

  hasSelectedDefaultIcon_ = false;
  FolderItemSP folderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(itemId);

  if (!folderItem.get()) {
    if (itemId == ID_MENU_DefaultGroupIcon) {
      hasSelectedDefaultIcon_ = true;
      iconBitmap->SetBitmap(wxBitmap(FilePaths::GetSkinFile(_T("FolderIcon32.png")), wxBITMAP_TYPE_PNG));
    } else {
      evt.Skip();
    }
  } else {
    wxIconSP icon = folderItem->GetIcon(LARGE_ICON_SIZE);
    wxBitmap bitmap(*icon);
    iconBitmap->SetBitmap(bitmap);
    iconFolderItem_ = folderItem;
  }
}


void GroupEditorDialog::OnChangeIconButtonClick(wxCommandEvent& evt) {
  wxMenu menu;

  wxMenuItem* defaultMenuItem = new wxMenuItem(&menu, ID_MENU_DefaultGroupIcon, _("Default"));
  wxBitmap iconBitmap(FilePaths::GetSkinFile(_T("FolderIcon16.png")), wxBITMAP_TYPE_PNG);
  defaultMenuItem->SetBitmap(iconBitmap);

  menu.Append(defaultMenuItem);
  menu.AppendSeparator();

  for (int i = 0; i < folderItem_->ChildrenCount(); i++) {
    FolderItemSP child = folderItem_->GetChildAt(i);
    child->AppendAsMenuItem(&menu);
  }

  menu.Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(GroupEditorDialog::OnMenuItemClick),
    NULL,
    this);

  PopupMenu(&menu, wxPoint(changeIconButton->GetRect().GetX(), changeIconButton->GetRect().GetBottom()));
}


void GroupEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_CANCEL);
}


void GroupEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  folderItem_->SetName(nameTextBox->GetValue());

  if (hasSelectedDefaultIcon_) {
    FolderItemSP nullFolderItem;
    folderItem_->SetGroupIcon(nullFolderItem);
  } else {
    if (iconFolderItem_.get()) {
      folderItem_->SetGroupIcon(iconFolderItem_);
    }
  }

  EndDialog(wxID_OK);
}


void GroupEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  wxIconSP icon = folderItem_->GetIcon(LARGE_ICON_SIZE);
  wxBitmap bitmap(*icon);
  iconBitmap->SetBitmap(bitmap);
}


void GroupEditorDialog::OnShow(wxShowEvent& evt) {
  
}