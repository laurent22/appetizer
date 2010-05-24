/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef IconPanel_H
#define IconPanel_H

#include <GraphicsItem.h>
#include <NineSlicePainter.h>
#include <FolderItem.h>

namespace appetizer {

class IconPanel : public GraphicsItem {

public:

  IconPanel();
  void loadFolderItems(int rootFolderItemId);
  FolderItem* rootFolderItem();
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:

  int rootFolderItemId_;

};

}
#endif // NineSliceItem_H
