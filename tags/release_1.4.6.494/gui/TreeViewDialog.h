/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __TreeViewDialog_H
#define __TreeViewDialog_H

#include "TreeViewDialogBase.h"
#include "../User.h"
#include "../FolderItem.h"


class FolderItemTreeItemData: public wxTreeItemData {

public:

  FolderItemTreeItemData(appFolderItem* folderItem);
  appFolderItem* GetFolderItem();

private:

  appFolderItem* folderItem_;

};


class TreeViewDialog: public TreeViewDialogBase {

public:

  TreeViewDialog(wxWindow* parent);
  ~TreeViewDialog();
  void Localize();
  void LoadFolderItem(appFolderItem* folderItem);
  void SelectAndExpandFolderItem(appFolderItem* folderItem);

  void OnTreeBeginDrag(wxTreeEvent& evt);
  void OnTreeEndDrag(wxTreeEvent& evt);
  void OnButton(wxCommandEvent& evt);
  void OnItemRightClick(wxTreeEvent& evt);
  void OnMenuItemClick(wxCommandEvent& evt);

private:

  wxTreeItemId PrependFolderItem(const wxTreeItemId& parent, appFolderItem* folderItem);
  wxTreeItemId InsertFolderItemAfter(const wxTreeItemId& parent, appFolderItem* folderItem, const wxTreeItemId& previous);
  wxTreeItemId AppendFolderItem(const wxTreeItemId& parent, appFolderItem* folderItem);
  wxTreeItemId GetTreeItemFromFolderItem(wxTreeItemId startItemId, appFolderItem* folderItem);
  void SetItemImage(const wxTreeItemId& item, appFolderItem* folderItem);

  wxTreeItemId draggedTreeItemId_;
  wxImageList* imageList_;
  wxStopWatch lastDropEventStopWatch_;

  DECLARE_EVENT_TABLE()

};


#endif // __TreeViewDialog_H