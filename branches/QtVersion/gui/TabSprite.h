/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_TabSprite_H
#define Appetizer_TabSprite_H

#include <FolderItem.h>
#include <GraphicsItem.h>
#include <NineSliceItem.h>

namespace appetizer {

class TabSprite : public GraphicsItem {

public:

  TabSprite();
  void applySkin();
  void loadFolderItem(int folderItemId);
  FolderItem* folderItem();
  void updateDisplay();

private:

  NineSliceItem* backgroundSprite_;
  QGraphicsSimpleTextItem* textSprite_;
  int folderItemId_;

};

}
#endif // Appetizer_TabSprite_H
