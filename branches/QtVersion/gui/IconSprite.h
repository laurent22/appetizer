/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_IconSprite_H
#define Appetizer_IconSprite_H

#include <FolderItem.h>
#include <GraphicsItem.h>

namespace appetizer {

class IconSprite : public GraphicsItem {

  Q_OBJECT

public:

  IconSprite();
  FolderItem* folderItem() const;
  void setFolderItem(int folderItemId);
  int size() const;
  void setSize(int size);
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

public slots:

  void folderItem_iconLoaded(int iconSize);

private:

  int folderItemId_;
  int size_;

};

}
#endif // Appetizer_IconSprite_H
