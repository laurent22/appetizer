/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef MainScene_H
#define MainScene_H

#include <MainPanel.h>

namespace appetizer {

class MainScene : public QGraphicsScene {

public:

  MainScene();

protected:

  void drawBackground(QPainter* painter, const QRectF& rect);
  QBitmap* composedMask();

private:

  MainPanel* mainPanel_;
  QBitmap* composedMask_;

};

}
#endif // MainScene_H
