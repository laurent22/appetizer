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


class Utilities {

public:

  Utilities();
  ~Utilities();
  void EjectDriveAndExit(bool askForConfirmation = true);
  bool DoMultiLaunch();
  void ShowConfigDialog();
  void ShowHelpFile(const wxString& anchor = wxEmptyString);
  void ShowAboutDialog();
  void ShowTreeViewDialog(int selectedFolderItemId = -1);
  void CreateNewShortcut();
  bool IsApplicationOnRemoteDrive();
  void InstallAutorunFile();
  wxString CreateUUID();
  void Localize();

private:

  AboutDialog* aboutDialog_;
  ConfigDialog* configDialog_;
  TreeViewDialog* treeViewDialog_;

};

#endif // __Utilities_H