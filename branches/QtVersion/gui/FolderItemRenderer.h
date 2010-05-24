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

namespace appetizer {

class FolderItemRenderer : public GraphicsItem {

public:

  FolderItemRenderer();
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:
};

}
#endif // FolderItemRenderer_H
