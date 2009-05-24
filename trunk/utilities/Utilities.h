/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __Utilities_H
#define __Utilities_H


#include "../gui/ConfigDialog.h"
#include "../gui/AboutDialog.h"
#include "../gui/TreeViewDialog.h"
#include "../FolderItem.h"


class Utilities {

public:

  Utilities();
  ~Utilities();
  void ShowEjectDriveDialog();
  void EjectDriveAndExit(bool askForConfirmation = true);
  bool DoMultiLaunch();
  void ShowConfigDialog();
  void ShowHelpFile(const wxString& anchor = wxEmptyString);
  void ShowAboutDialog();
  void ShowTreeViewDialog(int selectedFolderItemId = -1);
  void ShowImportDialog();
  void CreateNewShortcut();
  bool RemoveFolderItemWithConfirmation(FolderItem* folderItem);
  bool IsApplicationOnPortableDrive();
  bool InstallAutorunFile();
  void KillLockingProcesses(const wxString& drive, bool painless = true);
  void CreateAndRunVBScript(const wxString& filePath, const wxString& script);
  wxString CreateUUID();
  void SwitchSkin(const wxString& skinName);
  wxArrayString GetSkinNames();
  void ConvertStaticTextToLink(wxStaticText* label);
  void Localize();
  void CreateShortcut(const wxString& filePath, const wxString& shortcutPath, const wxString& iconPath, int iconIndex);

private:

  AboutDialog* aboutDialog_;
  ConfigDialog* configDialog_;
  TreeViewDialog* treeViewDialog_;

};

#endif // __Utilities_H