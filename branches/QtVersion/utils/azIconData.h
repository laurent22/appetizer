/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stable.h"

#ifndef AZICONDATA_H
#define AZICONDATA_H

class azIconData {

public:

    azIconData();
    std::wstring filePath;
    int index;
    HICON hIcon;
    bool isNull() const;

};

#endif // AZICONDATA_H
