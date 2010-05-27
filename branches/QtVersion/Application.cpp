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

  #ifdef __WINDOWS__
  osInfo_.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  BOOL gotInfo = GetVersionEx(&osInfo_);
  if (!gotInfo) {
    osInfo_.dwMajorVersion = 5; // Assume Windows 2000
    osInfo_.dwMinorVersion = 0;
  }  
  #endif // __WINDOWS__


  FilePaths::InitializePaths();
  FilePaths::CreateSettingsDirectory();

  Style::loadSkinFile("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Skin.xml");

  user_.load();

  mainWindow_ = new MainWindow();
  mainWindow_->show();
}


OSVERSIONINFO Application::osInfo() {
  return osInfo_;
}


User Application::user() const {
  return user_;
}


Application* Application::instance() {
  Application* a = static_cast<Application*>(QApplication::instance());
  return a;
}


int Application::getValidIconSize(int requiredIconSize) const {
  #ifdef __WINDOWS__

  int major = osInfo_.dwMajorVersion;
  int minor = osInfo_.dwMinorVersion;

  if (major < 5) {

    // Before Windows 2000
    if (requiredIconSize > 32) return 32;

  } else if (major == 5) {

    if (minor < 1) {
      // Windows 2000
      if (requiredIconSize > 32) return 32;
    } else {
      // Windows XP
      if (requiredIconSize > 48) return 48;
    }

  } else {
    
    // Vista and above
    if (requiredIconSize > 256) return 256;
  }

  #endif // __WINDOWS__

  return requiredIconSize;
}


int Application::getNextValidIconSize(int requiredIconSize) const {
  if (requiredIconSize <= SMALL_ICON_SIZE) return Application::instance()->getValidIconSize(SMALL_ICON_SIZE);
  if (requiredIconSize <= MEDIUM_ICON_SIZE) return Application::instance()->getValidIconSize(MEDIUM_ICON_SIZE);
  if (requiredIconSize <= LARGE_ICON_SIZE) return Application::instance()->getValidIconSize(LARGE_ICON_SIZE);
  if (requiredIconSize <= EXTRA_LARGE_ICON_SIZE) return Application::instance()->getValidIconSize(EXTRA_LARGE_ICON_SIZE);
  if (requiredIconSize <= JUMBO_ICON_SIZE) return Application::instance()->getValidIconSize(JUMBO_ICON_SIZE);

  return Application::instance()->getValidIconSize(JUMBO_ICON_SIZE);
}