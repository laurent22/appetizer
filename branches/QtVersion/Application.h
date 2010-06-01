/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FolderItem.h>
#include <MainWindow.h>
#include <UserSettings.h>

#ifndef Application_H
#define Application_H
namespace appetizer {

class Application : public QApplication {

public:

  Application(int argc, char *argv[]);
  MainWindow mainWindow() const;
  UserSettings settings() const;
  int getValidIconSize(int requiredIconSize) const;
  int getNextValidIconSize(int requiredIconSize) const;
  FolderItem* rootFolderItem() const;

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo();
  #endif // __WINDOWS__

  static Application* instance();

private:

  MainWindow* mainWindow_;
  UserSettings settings_;
  FolderItem* rootFolderItem_;
  void loadFolderItems();

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo_;
  #endif // __WINDOWS__

};

}
#endif // Application_H
