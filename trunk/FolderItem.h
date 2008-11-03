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


class FolderItem {

public:

  FolderItem(bool isGroup = false);
  ~FolderItem();
  int GetId() const;
  void AutoSetName();
  wxString GetName();
  wxString GetResolvedPath();
  wxString GetFilePath();
  wxIconSP GetIcon(int iconSize);
  void SetFilePath(const wxString& filePath);
  void SetName(const wxString& name);  
  void ClearCachedIcons();  
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

  wxMenuItem* ToMenuItem(wxMenu* parentMenu);

  static wxString ResolvePath(const wxString& filePath);
  static wxString ConvertToRelativePath(const wxString& filePath);

  bool IsGroup();
  FolderItemVector GetChildren();
  void AddChild(FolderItemSP folderItem);
  FolderItemSP GetParent();
  void SetParent(FolderItemSP folderItem);
  void RemoveChild(FolderItemSP folderItem);
  FolderItemSP GetChildAt(int index);
  int ChildrenCount();
  FolderItemSP GetChildById(int folderItemId, bool recurse = true);

private:

  static int uniqueID_;

  bool isGroup_;
  FolderItemSP parent_;
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