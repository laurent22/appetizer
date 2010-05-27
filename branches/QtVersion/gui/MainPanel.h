/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_MainPanel_H
#define Appetizer_MainPanel_H

#include <GraphicsItem.h>
#include <IconPanel.h>
#include <NineSliceItem.h>

namespace appetizer {

class MainPanel : public GraphicsItem {

public:

  MainPanel();

private:

  NineSliceItem* backgroundItem_;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
  IconPanel* iconPanel_;

};

}
#endif // Appetizer_MainPanel_H
