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

  FolderItemTreeItemData(FolderItem* folderItem);
  FolderItem* GetFolderItem();

private:

  FolderItem* folderItem_;

};


class TreeViewDialog: public TreeViewDialogBase {

public:

  TreeViewDialog();
  ~TreeViewDialog();
  void Localize();
  void LoadFolderItem(FolderItem* folderItem);
  void SelectAndExpandFolderItem(FolderItem* folderItem);

  void OnTreeBeginDrag(wxTreeEvent& evt);
  void OnTreeEndDrag(wxTreeEvent& evt);
  void OnButton(wxCommandEvent& evt);

private:

  wxTreeItemId PrependFolderItem(const wxTreeItemId& parent, FolderItem* folderItem);
  wxTreeItemId InsertFolderItemAfter(const wxTreeItemId& parent, FolderItem* folderItem, const wxTreeItemId& previous);
  wxTreeItemId AppendFolderItem(const wxTreeItemId& parent, FolderItem* folderItem);
  wxTreeItemId GetTreeItemFromFolderItem(wxTreeItemId startItemId, FolderItem* folderItem);
  void SetItemImage(const wxTreeItemId& item, FolderItem* folderItem);

  wxTreeItemId draggedTreeItemId_;
  wxImageList* imageList_;
  wxStopWatch lastDropEventStopWatch_;

  DECLARE_EVENT_TABLE()

};


#endif // __TreeViewDialog_H