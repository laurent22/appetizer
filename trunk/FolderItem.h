/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __FolderItem_H
#define __FolderItem_H

#include <wx/wx.h>
#include <wx/menuitem.h>
#include "boost/shared_ptr.hpp"
#include "TypeDefinitions.h"
#include "utilities/XmlUtil.h"


class FolderItem {

public:

  FolderItem();
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
  TiXmlElement* ToXML();
  void FromXML(TiXmlElement* xml);

  bool GetAutomaticallyAdded();
  void SetAutomaticallyAdded(bool automaticallyAdded);

  void AddToMultiLaunchGroup();
  void RemoveFromMultiLaunchGroup();
  bool BelongsToMultiLaunchGroup();

  wxMenuItem* ToMenuItem(wxMenu* parentMenu);

  static wxString ResolvePath(const wxString& filePath);
  static wxString ConvertToRelativePath(const wxString& filePath);

private:

  static int uniqueID_;

  int id_;
  bool belongsToMultiLaunchGroup_;
  wxString name_;
  wxString filePath_;
  wxIconSP icon16_;
  wxIconSP icon32_;  
  bool automaticallyAdded_;

};


typedef boost::shared_ptr<FolderItem> FolderItemSP;


#endif // __FolderItem_H