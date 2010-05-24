/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef MainScene_H
#define MainScene_H

#include <NineSliceItem.h>
#include <IconPanel.h>

namespace appetizer {

class MainScene : public QGraphicsScene {

public:

  MainScene();

private:

  NineSliceItem* backgroundItem_;
  void drawBackground(QPainter* painter, const QRectF& rect);
  IconPanel* iconPanel_;

};

}
#endif // MainScene_H
