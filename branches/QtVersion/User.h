/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_User_H
#define Appetizer_User_H

#include <FolderItem.h>

namespace appetizer {

class User {

public:

    User();
    void load();
    void synchronizeWithFolder(const QString& path);
    FolderItem* rootFolderItem() const;

private:

  FolderItem* rootFolderItem_;

};

}
#endif // Appetizer_User_H
