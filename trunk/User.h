/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __User_H
#define __User_H

#include "FolderItem.h"
#include "Enumerations.h"
#include "UserSettings.h"
#include "gui/ShortcutEditorDialog.h"


class User : public wxEvtHandler {

public:

  User();  
  ~User();  
  UserSettingsSP GetSettings();
  void ScheduleSave();
  void Save(bool force = false);
  void Load();

  void GetShortcutsFromFolder(const wxString& folderPath, wxArrayString* result);
  
  void PortableAppsFormatSynchronization();
  void StartMenuSynchronization(wxProgressDialog* progressDialog = NULL);
  void QuickLaunchSynchronization();

  int EditFolderItem(FolderItemSP folderItem);
  FolderItemSP EditNewFolderItem(FolderItemSP parent, bool isGroup = false);
  FolderItemSP AddNewFolderItemFromPath(FolderItemSP parent, wxString folderItemPath);
  void AddAutoAddExclusion(const wxString& filePath);
  bool IsAutoAddExclusion(const wxString& filePath);

  void BatchAddFolderItems(const wxArrayString& filePaths, bool useAutoAddExclusions = false, wxProgressDialog* progressDialog = NULL);

  FolderItemSP GetRootFolderItem();

  void OnTimer(wxTimerEvent& evt);

private:

  FolderItemSP rootFolderItem_;
  wxTimer* scheduledSaveTimer_;
  wxStringList folderItemExclusions_;
  UserSettingsSP settings_;
  ShortcutEditorDialog* shortcutEditorDialog_;
  wxArrayString autoAddExclusions_;

  DECLARE_EVENT_TABLE()

};


#endif