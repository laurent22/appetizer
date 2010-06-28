/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FolderItem.h>
#include <MainWindow.h>


#ifndef Application_H
#define Application_H
namespace appetizer {

class Application : public QApplication {

  Q_OBJECT

public:

  Application(int argc, char *argv[]);
  ~Application();
  inline MainWindow* mainWindow() { return mainWindow_; };
  int getValidIconSize(int requiredIconSize) const;
  int getNextValidIconSize(int requiredIconSize) const;
  FolderItem* rootFolderItem() const;
  void scheduleClose();
  void scheduleMinimize();
  inline bool isClosing() const { return closingTimer_ != NULL; }
  inline bool isMinimizing() const { return minimizingTimer_ != NULL; }

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo();
  #endif // __WINDOWS__

  static Application* instance();

private:

  MainWindow* mainWindow_;
  FolderItem* rootFolderItem_;
  void loadFolderItems();
  QTimer* closingTimer_;
  QTimer* minimizingTimer_;

  #ifdef __WINDOWS__
  OSVERSIONINFO osInfo_;
  #endif // __WINDOWS__

private slots:

  void closingTimer_timeout();
  void minimizingTimer_timeout();

};

}
#endif // Application_H
