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
  void drawMask(QPainter* painter, int x, int y, int width, int height);

protected:

  void drawBackground(QPainter* painter, const QRectF& rect);

private:

  MainPanel* mainPanel_;

};

}
#endif // MainScene_H
