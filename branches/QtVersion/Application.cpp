/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <FilePaths.h>

using namespace appetizer;

Application::Application(int argc, char *argv[]) : QApplication(argc, argv) {
  FilePaths::InitializePaths();
  FilePaths::CreateSettingsDirectory();

  user_.load();

  mainWindow_.show();


}
