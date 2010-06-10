/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef FolderItemRenderer_H
#define FolderItemRenderer_H

#include <FolderItem.h>
#include <GraphicsWindow.h>
#include <IconSprite.h>
#include <NineSliceItem.h>
#include <NineSlicePainter.h>

namespace appetizer {

class FolderItemSprite : public GraphicsItem {

public:

  FolderItemSprite(GraphicsWindow* parentWindow);

  void setFolderItem(int folderItemId);
  FolderItem* folderItem() const;
  void setIconSize(int size);
  int iconSize() const;
  void updateDisplay();

protected:

  void hoverEnterEvent(QGraphicsSceneHoverEvent* event); 
  void hoverLeaveEvent(QGraphicsSceneHoverEvent* event);

private:

  int folderItemId_;
  int iconSize_;
  NineSliceItem* selectionSprite_;
  IconSprite* iconSprite_;
  bool mouseInside_;
  QPropertyAnimation* selectionSpriteAnimation_;

};

}
#endif // FolderItemRenderer_H
