/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef MainWindow_H
#define MainWindow_H

#include <MainScene.h>

namespace appetizer {

class MainWindow : public QGraphicsView {

public:

  MainWindow();

private:

  MainScene* scene_;

protected:

  void resizeEvent(QResizeEvent* event);

};

}
#endif // MainWindow_H
