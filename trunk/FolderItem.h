/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __FolderItem_H
#define __FolderItem_H

#include <wx/wx.h>
#include <wx/menuitem.h>
#include <vector>
#include "boost/shared_ptr.hpp"
#include "TypeDefinitions.h"
#include "utilities/XmlUtil.h"


// Forward declaration so that we can create the typedef below
class FolderItem;

typedef boost::shared_ptr<FolderItem> FolderItemSP;
typedef std::vector<FolderItemSP> FolderItemVector;


class FolderItem : public wxEvtHandler {

public:

  FolderItem(bool isGroup = false);
  ~FolderItem();
  int GetId() const;
  void AutoSetName();
  wxString GetName(bool returnUnnamedIfEmpty = false);
  wxString GetResolvedPath();
  wxString GetFileName(bool includeExtension = true);
  wxString GetFilePath();
  wxIconSP GetIcon(int iconSize);
  void SetFilePath(const wxString& filePath);
  void SetName(const wxString& name);  
  void ClearCachedIcons();  
  void AppendAsMenuItem(wxMenu* parentMenu);
  wxMenu* ToMenu();
  wxMenuItem* ToMenuItem(wxMenu* parentMenu);
  void Launch();
  void LaunchWithArguments(const wxString& arguments);
  static void Launch(const wxString& filePath, const wxString& arguments = wxEmptyString);
  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  bool GetAutomaticallyAdded();
  void SetAutomaticallyAdded(bool automaticallyAdded);

  void AddToMultiLaunchGroup();
  void RemoveFromMultiLaunchGroup();
  bool BelongsToMultiLaunchGroup();  

  static wxString ResolvePath(const wxString& filePath);
  static wxString ConvertToRelativePath(const wxString& filePath);
  void DoMultiLaunch();

  // ***************************************************************
  // Methods to work with children
  // ***************************************************************
  bool IsGroup();
  FolderItemVector GetChildren();  
  FolderItem* GetParent();
  void SetParent(FolderItem* folderItem);
  void RemoveChild(FolderItemSP folderItem);
  void MoveChild(FolderItemSP folderItemToMove, int insertionIndex);
  FolderItemSP SearchChildByFilename(const wxString& filename, int matchMode = 2);

  int ChildrenCount();

  FolderItemSP GetChildAt(int index);
  FolderItemSP GetChildById(int folderItemId, bool recurse = true);
  FolderItemSP GetChildByResolvedPath(const wxString& filePath);

  void InsertChildBefore(FolderItemSP toAdd, FolderItemSP previousFolderItem);
  void InsertChildAfter(FolderItemSP toAdd, FolderItemSP previousFolderItem);
  void PrependChild(FolderItemSP toAdd);
  void AddChild(FolderItemSP folderItem);

  void OnMenuItemClick(wxCommandEvent& evt);

private:

  static int uniqueID_;

  bool isGroup_;
  FolderItem* parent_;
  int id_;
  FolderItemVector children_;
  bool belongsToMultiLaunchGroup_;
  wxString name_;
  wxString filePath_;
  wxIconSP icon16_;
  wxIconSP icon32_;  
  bool automaticallyAdded_;

};





#endif // __FolderItem_H