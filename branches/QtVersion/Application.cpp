/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <FilePaths.h>
#include <Style.h>

using namespace appetizer;

Application::Application(int argc, char *argv[]) : QApplication(argc, argv) {
  FilePaths::InitializePaths();
  FilePaths::CreateSettingsDirectory();

  Style::loadSkinFile("s:\\Docs\\PROGS\\C++\\Appetizer\\branches\\QtVersion\\Data\\Skin\\Default\\Skin.xml");

  user_.load();

  mainWindow_ = new MainWindow();
  mainWindow_->show();


}


User Application::user() const {
  return user_;
}


Application* Application::instance() {
  Application* a = static_cast<Application*>(QApplication::instance());
  return a;
}