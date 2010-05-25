/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef FolderItemRenderer_H
#define FolderItemRenderer_H

#include <GraphicsItem.h>
#include <NineSlicePainter.h>
#include <FolderItem.h>

namespace appetizer {

class FolderItemRenderer : public GraphicsItem {

public:

  FolderItemRenderer();
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
  void setFolderItem(int folderItemId);
  FolderItem* folderItem() const;
  void setIconSize(int size);
  int iconSize() const;
  int validIconSize() const;

private:

  int folderItemId_;
  int iconSize_;

};

}
#endif // FolderItemRenderer_H
