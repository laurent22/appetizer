/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainWindow.h>
#include <User.h>

#ifndef Application_H
#define Application_H
namespace appetizer {

class Application : public QApplication {

public:

  Application(int argc, char *argv[]);
  MainWindow mainWindow();
  User user();

private:

  MainWindow mainWindow_;
  User user_;

};

}
#endif // Application_H
