/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "TreeViewDialog.h"
#include "../Constants.h"
#include "../TypeDefinitions.h"
#include "../MiniLaunchBar.h"


BEGIN_EVENT_TABLE(TreeViewDialog, wxDialog)
  EVT_TREE_BEGIN_DRAG(ID_TREEVIEW_DLG_treeControl, TreeViewDialog::OnTreeBeginDrag)
  EVT_TREE_END_DRAG(ID_TREEVIEW_DLG_treeControl, TreeViewDialog::OnTreeEndDrag)
  EVT_BUTTON(wxID_ANY, TreeViewDialog::OnButton)
  EVT_TREE_ITEM_RIGHT_CLICK(wxID_ANY, TreeViewDialog::OnItemRightClick)
  EVT_MENU(wxID_ANY, TreeViewDialog::OnMenuItemClick)
END_EVENT_TABLE()


FolderItemTreeItemData::FolderItemTreeItemData(appFolderItem* folderItem) {
  folderItem_ = folderItem;
}


appFolderItem* FolderItemTreeItemData::GetFolderItem() {
  return folderItem_;
}


TreeViewDialog::TreeViewDialog(wxWindow* parent)
: TreeViewDialogBase(parent, wxID_ANY, wxEmptyString) {
  
  imageList_ = NULL;  
}


void TreeViewDialog::OnButton(wxCommandEvent& evt) {
  int buttonId = evt.GetId();

  switch (buttonId) {

    case ID_TREEVIEW_DLG_closeButton:

      EndDialog(wxID_CLOSE);
      break;

    default:

      evt.Skip();
      break;

  }
}


void TreeViewDialog::OnMenuItemClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  if (!menuItem) return;
  
  int folderItemId = menuItem->GetMetadataInt(_T("folderItemId"));
  appFolderItem* folderItem = appFolderItem::GetFolderItemById(folderItemId);
  if (!folderItem) return;

  wxTreeItemId treeItemId = GetTreeItemFromFolderItem(treeControl->GetRootItem(), folderItem);
  if (!treeItemId.IsOk()) return;

  wxString name = menuItem->GetMetadata(_T("name"));

  if (name == _T("remove")) {

    bool done = wxGetApp().GetUtilities().RemoveFolderItemWithConfirmation(folderItem);
    if (done) {
      treeControl->Delete(treeItemId);
      wxGetApp().FolderItems_CollectionChange();
    }

  } else if (name == _T("properties")) {

    wxGetApp().GetUser()->EditFolderItem(folderItem);
    treeControl->SetItemText(treeItemId, folderItem->GetName());
    wxGetApp().FolderItems_FolderItemChange(folderItem);

  } else {

    evt.Skip();

  }
}


void TreeViewDialog::OnItemRightClick(wxTreeEvent& evt) {
  wxTreeItemId item = evt.GetItem();
  if (!item.IsOk()) return;

  appFolderItem* folderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(item))->GetFolderItem();
  if (!folderItem) return;

  wxMenu* menu = new wxMenu();

  ExtendedMenuItem* menuItem = NULL;
  
  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Remove..."));
  menuItem->SetMetadata(_T("name"), _T("remove"));
  menuItem->SetMetadata(_T("folderItemId"), folderItem->GetId());
  menu->Append(menuItem);

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Properties"));
  menuItem->SetMetadata(_T("name"), _T("properties"));
  menuItem->SetMetadata(_T("folderItemId"), folderItem->GetId());
  menu->Append(menuItem); 

  PopupMenu(menu, wxDefaultPosition);

  wxDELETE(menu);
}


TreeViewDialog::~TreeViewDialog() {
  treeControl->SetImageList(NULL);
  wxDELETE(imageList_);
}


void TreeViewDialog::Localize() {
  SetTitle(_("Organize shortcuts"));
  closeButton->SetLabel(_("Close"));
}


void TreeViewDialog::SelectAndExpandFolderItem(appFolderItem* folderItem) {
  wxTreeItemId item = GetTreeItemFromFolderItem(treeControl->GetRootItem(), folderItem);
  if (!item.IsOk()) {
    ELOG(_T("SelectAndExpandFolderItem: item is not ok"));
    return;
  }

  treeControl->Expand(item);
  treeControl->SelectItem(item);
}


wxTreeItemId TreeViewDialog::GetTreeItemFromFolderItem(wxTreeItemId startItemId, appFolderItem* folderItem) {
  if (!startItemId.IsOk()) {
    ELOG(_T("GetTreeItemFromFolderItem: startItemId is not ok"));
    return wxTreeItemId();
  }
  
  appFolderItem* startFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(startItemId))->GetFolderItem();
  if (startFolderItem->GetId() == folderItem->GetId()) return startItemId;

  wxTreeItemIdValue cookie;
  wxTreeItemId child = treeControl->GetFirstChild(startItemId, cookie);
  while (child.IsOk()) {
    appFolderItem* childFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(child))->GetFolderItem();
    if (childFolderItem->GetId() == folderItem->GetId()) return child;
    
    wxTreeItemId foundTreeItem = GetTreeItemFromFolderItem(child, folderItem);
    if (foundTreeItem.IsOk()) return foundTreeItem;

    child = treeControl->GetNextChild(startItemId, cookie);
  }

  wxTreeItemId notFoundId;
  return notFoundId;
}


void TreeViewDialog::LoadFolderItem(appFolderItem* folderItem) {
  Localize();

  FolderItemVector folderItems = folderItem->GetChildren();

  if (imageList_) {
    imageList_->RemoveAll();
  } else {
    imageList_ = new wxImageList(SMALL_ICON_SIZE, SMALL_ICON_SIZE);
  }
  
  treeControl->DeleteAllItems();
  treeControl->SetImageList(NULL);

  wxTreeItemId rootId = treeControl->AddRoot(_T("Applications"));
  treeControl->SetItemData(rootId, new FolderItemTreeItemData(folderItem));
  
  for (int i = 0; i < folderItems.size(); i++) {
    appFolderItem* folderItem = folderItems.at(i);
    AppendFolderItem(rootId, folderItem);
  }

  treeControl->SetImageList(imageList_);
  treeControl->Expand(rootId);
}


wxTreeItemId TreeViewDialog::PrependFolderItem(const wxTreeItemId& parent, appFolderItem* folderItem) {
  if (!parent.IsOk()) return wxTreeItemId();

  wxTreeItemId firstId = treeControl->PrependItem(
    parent,
    folderItem->GetName(true),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  SetItemImage(firstId, folderItem);

  if (!folderItem->IsGroup()) return firstId;

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    appFolderItem* child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


wxTreeItemId TreeViewDialog::InsertFolderItemAfter(const wxTreeItemId& parent, appFolderItem* folderItem, const wxTreeItemId& previous) {
  if (!parent.IsOk()) return wxTreeItemId();

  wxTreeItemId firstId = treeControl->InsertItem(
    parent,
    previous,
    folderItem->GetName(true),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  SetItemImage(firstId, folderItem);

  if (!folderItem->IsGroup()) return firstId;

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    appFolderItem* child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


wxTreeItemId TreeViewDialog::AppendFolderItem(const wxTreeItemId& parent, appFolderItem* folderItem) {
  if (!parent.IsOk()) return wxTreeItemId();

  wxTreeItemId firstId = treeControl->AppendItem(
    parent,
    folderItem->GetName(true),
    -1,
    -1,
    new FolderItemTreeItemData(folderItem));

  SetItemImage(firstId, folderItem);

  if (!folderItem->IsGroup()) return firstId;

  for (int i = 0; i < folderItem->ChildrenCount(); i++) {
    appFolderItem* child = folderItem->GetChildAt(i);
    AppendFolderItem(firstId, child);
  }

  return firstId;
}


void TreeViewDialog::SetItemImage(const wxTreeItemId& item, appFolderItem* folderItem) {
  if (!item.IsOk()) return;

  wxIcon* icon = folderItem->GetIcon(SMALL_ICON_SIZE);
  int imageIndex = imageList_->Add(*icon);
  treeControl->SetItemImage(item, imageIndex);
}


void TreeViewDialog::OnTreeBeginDrag(wxTreeEvent& evt) {
  if (lastDropEventStopWatch_.Time() < 300) return;

  draggedTreeItemId_ = evt.GetItem();
  if (!draggedTreeItemId_.IsOk()) return;
  
  evt.Allow();
}


void TreeViewDialog::OnTreeEndDrag(wxTreeEvent& evt) {
  wxTreeItemId targetItemId = evt.GetItem();
  if (!targetItemId.IsOk()) return;
  if (!draggedTreeItemId_.IsOk()) return;
  if (targetItemId == draggedTreeItemId_) return;

  appFolderItem* sourceFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(draggedTreeItemId_))->GetFolderItem();
  appFolderItem* targetFolderItem = ((FolderItemTreeItemData*)treeControl->GetItemData(targetItemId))->GetFolderItem();

  if (treeControl->GetRootItem() == targetItemId) {

    appFolderItem* sourceFolderItemParent = sourceFolderItem->GetParent();
    sourceFolderItemParent->RemoveChild(sourceFolderItem);
    sourceFolderItemParent = NULL;

    treeControl->Delete(draggedTreeItemId_);
    AppendFolderItem(targetItemId, sourceFolderItem); 
    targetFolderItem->AddChild(sourceFolderItem);

  } else {
  
    wxTreeItemId targetParentItemId = treeControl->GetItemParent(targetItemId);
    if (!targetParentItemId.IsOk()) return; 

    appFolderItem* sourceFolderItemParent = sourceFolderItem->GetParent();
    if (!sourceFolderItemParent) {
      ELOG(_T("Parent can't be NULL!"));
      return;
    }
    
    treeControl->Delete(draggedTreeItemId_);

    sourceFolderItemParent->RemoveChild(sourceFolderItem);
    sourceFolderItemParent = NULL;

    if (!targetFolderItem->IsGroup()) {
      wxTreeItemId previousSibling = treeControl->GetPrevSibling(targetItemId);
      if (previousSibling.IsOk()) {
        InsertFolderItemAfter(targetParentItemId, sourceFolderItem, previousSibling);
        targetFolderItem->GetParent()->InsertChildBefore(sourceFolderItem, targetFolderItem);
      } else {
        PrependFolderItem(targetParentItemId, sourceFolderItem);
        targetFolderItem->GetParent()->PrependChild(sourceFolderItem);
      }
    } else {
      targetFolderItem->AddChild(sourceFolderItem);
      AppendFolderItem(targetItemId, sourceFolderItem); 
    }
    
  }

  lastDropEventStopWatch_.Start();

  wxGetApp().FolderItems_CollectionChange();
}