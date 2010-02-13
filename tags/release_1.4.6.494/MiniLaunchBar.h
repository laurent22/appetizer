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
#include <ft2build.h>
#include FT_FREETYPE_H 


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
  appFolderItem* GetDraggedFolderItem();
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
  wxLocale* GetLocale();
  bool IsLaunchedOnStartup();
  void SetLaunchOnStartup(bool launch);

  const bool GetCommandLineFound(const wxString& name);

  void FolderItems_CollectionChange();
  void FolderItems_FolderItemChange(appFolderItem* folderItem);
  void User_LocaleChange();
  void User_IconSizeChange();

  FT_Library GetFreeTypeLibrary();
  FT_Face GetFreeTypeFace();

  #ifdef __WINDOWS__
  OSVERSIONINFO GetOsInfo();
  #endif // __WINDOWS__

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

  FT_Library ftLibrary_; 
  FT_Face ftFace_;

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo_;
  #endif // __WINDOWS__

};


DECLARE_APP(MiniLaunchBar)


#endif // __MiniLaunchBar_H