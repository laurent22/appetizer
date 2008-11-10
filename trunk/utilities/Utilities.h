/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __Utilities_H
#define __Utilities_H


#include <wx/wx.h>
#include "../gui/ConfigDialog.h"
#include "../gui/AboutDialog.h"
#include "../gui/TreeViewDialog.h"


class Utilities {

public:

  Utilities();
  ~Utilities();
  void EjectDriveAndExit(bool askForConfirmation = true);
  void DoMultiLaunch();
  void ShowConfigDialog();
  void ShowHelpFile();
  void ShowAboutDialog();
  void ShowTreeViewDialog(int selectedFolderItemId = -1);
  void CreateNewShortcut();
  bool IsApplicationOnRemoteDrive();
  wxString CreateUUID();
  void Localize();

private:

  AboutDialog* aboutDialog_;
  ConfigDialog* configDialog_;
  TreeViewDialog* treeViewDialog_;

};

#endif // __Utilities_H