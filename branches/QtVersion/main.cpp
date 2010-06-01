/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>

using namespace appetizer;

int main(int argc, char *argv[]) {
  Application app(argc, argv);
  app.setOrganizationName("Appetizer Project");
  app.setApplicationName("Appetizer");

  return app.exec();
}