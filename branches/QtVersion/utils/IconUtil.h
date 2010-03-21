/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef IconUtil_H
#define IconUtil_H

#include <IconData.h>

namespace appetizer {


// library function is: HRESULT SHGetImageList(int iImageList, REFIID riid, void **ppv)
typedef HRESULT (CALLBACK* SHGetImageListType)(int, const IID&, void*);

class IconUtil {

public:

  static IconData getFolderItemIcon(QString filePath, int iconSize);

};

}
#endif // IconUtil_H
