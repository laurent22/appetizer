/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stable.h"
#include "azIconData.h"

azIconData::azIconData() {
  hIcon = 0;
  index = 0;
}


bool azIconData::isNull() const {
  return filePath.empty() && !hIcon;
}
