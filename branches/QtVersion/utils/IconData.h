/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef IconData_H
#define IconData_H
namespace appetizer {

class IconData {

public:

    IconData();
    QString filePath;
    int index;
    HICON hIcon;
    bool isNull() const;

};

}
#endif // IconData_H
