/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stable.h"

#ifndef AZICONUTIL_H
#define AZICONUTIL_H

#include "azIconData.h"


// library function is: HRESULT SHGetImageList(int iImageList, REFIID riid, void **ppv)
typedef HRESULT (CALLBACK* SHGetImageListType)(int, const IID&, void*);

class azIconUtil {

public:

    static azIconData getFolderItemIcon(QString filePath, int iconSize);

};

#endif // AZICONUTIL_H
