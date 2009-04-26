/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __MiniLaunchBar_H
#define __MiniLaunchBar_H


#include "MainFrame.h"
#include "FolderItem.h"
#include "Enumerations.h"
class PluginManager;
#include "PluginManager.h"
#include "utilities/Utilities.h"


// Forward declarations:
class Utilities;
class MainFrame;



class MiniLaunchBar: public wxApp {

public:

  const wxCmdLineParser& GetCommandLine();
  MainFrame* GetMainFrame();
  Utilities& GetUtilities();
  long GetTimer();
  bool IsFirstLaunch();
  void SetDraggedFolderItem(int folderItemId);
  FolderItem* GetDraggedFolderItem();
  User* GetUser();
  void CheckForNewVersion(bool silent = false);
  bool ChangeLocale(const wxString& localeCode);
  void CloseApplication();
  int GetOSValidIconSize(int requiredIconSize);
  IntVector GetAllowedIconSizes();
  wxString GetIconSizeName(int iconSize);
  wxString GetContactEmail();
  PluginManager* GetPluginManager();
  int GetUniqueInt();
  void InitializePluginManager();

  const bool GetCommandLineFound(const wxString& name);

  void FolderItems_CollectionChange();
  void FolderItems_FolderItemChange(FolderItem* folderItem);
  void User_LocaleChange();
  void User_IconSizeChange();

private:

  static int uniqueInt_;
  
  virtual bool OnInit();
  virtual int OnExit();
  wxSingleInstanceChecker* singleInstanceChecker_;
  wxCmdLineParser commandLine_;
  wxCmdLineParser fileCommandLine_;
  MainFrame* mainFrame_;
  Utilities utilities_;
  wxStopWatch stopWatch_;
  bool isFirstLaunch_;
  int draggedFolderItemId_;
  User* user_;
  wxLocale* locale_;
  LongVector launchedProcessIds_;
  IntVector allowedIconSizes_;
  PluginManager* pluginManager_;

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo_;
  #endif

};


DECLARE_APP(MiniLaunchBar)


#endif // __MiniLaunchBar_H