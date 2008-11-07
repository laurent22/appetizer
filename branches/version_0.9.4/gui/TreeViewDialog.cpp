/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "TreeViewDialog.h"
#include "../Constants.h"
#include "../Localization.h"
#include "../TypeDefinitions.h"


BEGIN_EVENT_TABLE(TreeViewDialog, wxDialog)
  EVT_TREE_BEGIN_DRAG(ID_TREEVIEW_DLG_treeControl, TreeViewDialog::OnTreeBeginDrag)
  EVT_TREE_END_DRAG(ID_TREEVIEW_DLG_treeControl, TreeViewDialog::OnTreeEndDrag)
END_EVENT_TABLE()


FolderItemTreeItemData::FolderItemTreeItemData(FolderItemSP folderItem) {
  folderItem_ = folderItem;
}


FolderItemSP FolderItemTreeItemData::GetFolderItem() {
  return folderItem_;
}


TreeViewDialog::TreeViewDialog()
: TreeViewDialogBase(NULL, wxID_ANY, wxEmptyString) {
  
  imageList_ = NULL;

  Localize();
}


void TreeViewDialog::Localize() {

}


void TreeViewDialog::LoadUser(UserSP user) {
  LoadFolderItems(user->GetRootFolderItem()->GetChildren());
}


void TreeViewDialog::LoadFolderItems(FolderItemVector folderItems) {
  if (imageList_) {
    imageList_->RemoveAll();
  } else {
    imageList_ = new wxImageList(SMALL_ICON_SIZE, SMALL_ICON_SIZE);
  }

  wxTreeItemId rootId = treeControl->AddRoot(_T("Applications"));

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItemSP folderItem = folderItems.at(i);
    AppendFolderItem(rootId, folderItem);
  }

  treeControl->AssignImageList(imageList_);
  treeControl->Expand(rootId);
}


wxTreeItemId TreeViewDialog::PrependFolderItem(const wxTreeItemId& parent, FolderItemSP folderItem) {
  wxTreeItemId firstId = treeControl->PrependItem(
    parent,
    folderItem->GetName(),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  if (!folderItem->IsGroup()) {
    SetItemImage(firstId, folderItem);
    return firstId;
  }

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    FolderItemSP child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


wxTreeItemId TreeViewDialog::InsertFolderItemAfter(const wxTreeItemId& parent, FolderItemSP folderItem, const wxTreeItemId& previous) {
  wxTreeItemId firstId = treeControl->InsertItem(
    parent,
    previous,
    folderItem->GetName(),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  if (!folderItem->IsGroup()) {
    SetItemImage(firstId, folderItem);
    return firstId;
  }

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    FolderItemSP child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


wxTreeItemId TreeViewDialog::AppendFolderItem(const wxTreeItemId& parent, FolderItemSP folderItem) {
  wxTreeItemId firstId = treeControl->AppendItem(
    parent,
    folderItem->GetName(),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  if (!folderItem->IsGroup()) {
    SetItemImage(firstId, folderItem);
    return firstId;
  }

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    FolderItemSP child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


void TreeViewDialog::SetItemImage(const wxTreeItemId& item, FolderItemSP folderItem) {
  wxIconSP icon = folderItem->GetIcon(SMALL_ICON_SIZE);
  int imageIndex = imageList_->Add(*icon);
  treeControl->SetItemImage(item, imageIndex);
}


void TreeViewDialog::OnTreeBeginDrag(wxTreeEvent& evt) {
  draggedTreeItemId_ = evt.GetItem();
  evt.Allow();
}


void TreeViewDialog::OnTreeEndDrag(wxTreeEvent& evt) {
  wxTreeItemId targetItemId = evt.GetItem();
  if (!targetItemId.IsOk()) return;

  FolderItemSP sourceFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(draggedTreeItemId_))->GetFolderItem();
  
  if (treeControl->GetRootItem() == targetItemId) {



  } else {
  
    FolderItemSP targetFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(targetItemId))->GetFolderItem();
    
    treeControl->Delete(draggedTreeItemId_);

    if (!targetFolderItem->IsGroup()) {    
      wxTreeItemId previousSibling = treeControl->GetPrevSibling(targetItemId);
      if (previousSibling.IsOk()) {
        InsertFolderItemAfter(treeControl->GetItemParent(targetItemId), sourceFolderItem, previousSibling);
      } else {
        PrependFolderItem(treeControl->GetItemParent(targetItemId), sourceFolderItem);
      }
    } else {    
      AppendFolderItem(targetItemId, sourceFolderItem); 
    }

  }

}