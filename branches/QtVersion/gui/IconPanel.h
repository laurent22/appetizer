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
#include <FolderItemSprite.h>

namespace appetizer {

class IconPanel : public GraphicsItem {

public:

  IconPanel();
  void loadFolderItems(int rootFolderItemId);
  FolderItem* rootFolderItem();
  void updateDisplay();
  int defaultHeight() const;

protected:

  void resizeEvent();

private:

  int rootFolderItemId_;
  bool rebuildFolderItems_;
  std::vector<FolderItemSprite*> folderItemRenderers_;
  bool updateLayout_;
  int contentHeight_;

};

}
#endif // NineSliceItem_H
