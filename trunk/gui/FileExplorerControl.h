/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __FileExplorerControl_H
#define __FileExplorerControl_H



class FileExplorerControlItemData: public wxTreeItemData {

public:

  FileExplorerControlItemData(const wxString& filePath, bool isDirectory);
  wxString GetPath();
  bool IsDirectory();

private:

  wxString path_;
  bool isDirectory_;

};



class FileExplorerControl: public wxTreeCtrl {

DECLARE_DYNAMIC_CLASS(FileExplorerControl)

public:

  FileExplorerControl();
  FileExplorerControl(wxWindow* parent, wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxTR_HAS_BUTTONS | wxTR_HIDE_ROOT | wxTR_LINES_AT_ROOT, const wxValidator& validator = wxDefaultValidator, const wxString& name = _T("fileExplorerControl"));
  ~FileExplorerControl();
  void SetRootPath(const wxString& rootPath);
  void Localize();
  FileExplorerControlItemData* GetItemObject(const wxTreeItemId& itemId);
  void PopulateFolder(const wxTreeItemId& itemId);
  size_t wxGetAvailableDrives(wxArrayString &paths, wxArrayString &names, wxArrayInt &icon_ids);
  wxString GetSelectedPath();

  int OnCompareItems(const wxTreeItemId& item1, const wxTreeItemId& item2);
  void OnItemExpanding(wxTreeEvent& evt);

protected:

  wxTreeItemId rootId_;
  wxString rootPath_;
  wxImageList* imageList_;  

  DECLARE_EVENT_TABLE()

};


#endif // __FileExplorerControl_H