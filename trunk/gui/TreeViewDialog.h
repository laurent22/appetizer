/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __TreeViewDialog_H
#define __TreeViewDialog_H


#include <wx/wx.h>
#include <wx/treebase.h>
#include <wx/treectrl.h>
#include <vector>
#include <wx/imaglist.h>
#include "TreeViewDialogBase.h"
#include "../User.h"
#include "../FolderItem.h"


class FolderItemTreeItemData: public wxTreeItemData {

public:

  FolderItemTreeItemData(FolderItemSP folderItem);
  FolderItemSP GetFolderItem();

private:

  FolderItemSP folderItem_;

};


class TreeViewDialog: public TreeViewDialogBase {

public:

  TreeViewDialog();
  ~TreeViewDialog();
  void Localize();
  void LoadFolderItem(FolderItemSP folderItem);
  void SelectAndExpandFolderItem(FolderItemSP folderItem);

  void OnTreeBeginDrag(wxTreeEvent& evt);
  void OnTreeEndDrag(wxTreeEvent& evt);
  void OnButton(wxCommandEvent& evt);

private:

  wxTreeItemId PrependFolderItem(const wxTreeItemId& parent, FolderItemSP folderItem);
  wxTreeItemId InsertFolderItemAfter(const wxTreeItemId& parent, FolderItemSP folderItem, const wxTreeItemId& previous);
  wxTreeItemId AppendFolderItem(const wxTreeItemId& parent, FolderItemSP folderItem);
  wxTreeItemId GetTreeItemFromFolderItem(wxTreeItemId startItemId, FolderItemSP folderItem);
  void SetItemImage(const wxTreeItemId& item, FolderItemSP folderItem);

  wxTreeItemId draggedTreeItemId_;
  wxImageList* imageList_;

  DECLARE_EVENT_TABLE()

};


#endif // __TreeViewDialog_H