/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __FolderItem_H
#define __FolderItem_H


#include "Constants.h"
#include "TypeDefinitions.h"
#include "utilities/XmlUtil.h"


// Forward declaration so that we can create the typedef below
class FolderItem;
class FolderItemProcess;

typedef boost::shared_ptr<FolderItem> FolderItemSP;
typedef std::vector<FolderItemSP> FolderItemVector;


WX_DECLARE_HASH_MAP(int, wxIconSP, wxIntegerHash, wxIntegerEqual, IconHashMap);
WX_DECLARE_HASH_MAP(int, FolderItemSP, wxIntegerHash, wxIntegerEqual, FolderItemIdHashMap);


class FolderItem : public wxEvtHandler {

public:

  FolderItem(bool isGroup = false);
  ~FolderItem();
  static FolderItemSP CreateFolderItemSP(bool isGroup = false);
  static FolderItemSP GetFolderItemById(int id);
  void Dispose();
  bool IsDisposed();

  wxString GetUUID();
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

  static wxIconSP GetDefaultSpecialItemIcon(const wxString& path, int iconSize);
  static wxIconSP GetDefaultGroupIcon(int iconSize);

  wxString GetIconDiskCacheHash();
  static void CacheIconToDisk(const wxString& hash, wxIconSP icon, int iconSize);
  static wxIconSP GetIconFromDiskCache(const wxString& hash, int iconSize);

  void AppendAsMenuItem(wxMenu* parentMenu, int iconSize = SMALL_ICON_SIZE);
  wxMenu* ToMenu(int iconSize = SMALL_ICON_SIZE);
  wxMenuItem* ToMenuItem(wxMenu* parentMenu, int iconSize = SMALL_ICON_SIZE);

  void Launch();
  void LaunchWithArguments(const wxString& arguments);
  static void Launch(const wxString& filePath, const wxString& arguments = wxEmptyString);

  TiXmlElement* ToXml();
  void FromXml(TiXmlElement* xml);

  void SetParameters(const wxString& parameters);
  wxString GetParameters();

  bool GetAutomaticallyAdded();
  void SetAutomaticallyAdded(bool automaticallyAdded);

  void SetCustomIcon(const wxString& filePath, int index = 0);
  wxString GetCustomIconPath();
  int GetCustomIconIndex();

  void AddToMultiLaunchGroup();
  void RemoveFromMultiLaunchGroup();
  bool BelongsToMultiLaunchGroup();  

  static wxString ResolvePath(const wxString& filePath);
  static wxString ConvertToRelativePath(const wxString& filePath);
  bool DoMultiLaunch();

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
  FolderItemSP GetChildByUUID(const wxString& uuid, bool recurse = true);
  FolderItemSP GetChildByResolvedPath(const wxString& filePath);

  void InsertChildBefore(FolderItemSP toAdd, FolderItemSP previousFolderItem);
  void InsertChildAfter(FolderItemSP toAdd, FolderItemSP previousFolderItem);
  void PrependChild(FolderItemSP toAdd);
  void AddChild(FolderItemSP folderItem);

  void OnMenuItemClick(wxCommandEvent& evt);   

private:

  static int uniqueID_;

  wxString parameters_;
  bool isGroup_;
  FolderItem* parent_;
  int id_;
  FolderItemVector children_;
  bool belongsToMultiLaunchGroup_;
  wxString name_;
  wxString filePath_;
  IconHashMap icons_;
  wxString uuid_;
  bool automaticallyAdded_;
  wxString iconCacheHash_;
  wxString customIconPath_;
  int customIconIndex_;
  wxString resolvedPath_;
  bool isDisposed_;
  
  static FolderItemIdHashMap folderItemIdHashMap_;

};


#endif // __FolderItem_H